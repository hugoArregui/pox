;; Core units
(use irregex)

(import chicken scheme data-structures) 
(use spiffy srfi-1 extras ports intarweb spiffy-request-vars uri-common irregex)
(use awful awful-postgresql html-tags)
(use downtime)
(use pox-model pox-db/helpers pox-log pox-query)
(use http-session html-utils spiffy-cookies)

(enable-db)
(enable-session #t)
(generate-sxml? #t)

(define-logger log server)
(define-log-category auth)
(define-log-category session)
(define-log-category request)
(define-log-category headers)

(define (make-awful-response . args)
  (lambda ()
    (apply send-response args)))
  
(define (send-json-response doc)
  (make-awful-response 
    headers: '((content-type #(application/json ((charset . "utf-8")))))
    body: (json->string doc)))

(define (send-downtime-response body)
  (make-awful-response 
    headers: '((content-type #(text/x-downtime ((charset . "utf-8")))))
    body: body))

(define-syntax http-accept-case 
  (syntax-rules ()
    ((_ request (t1 e1 ...) (t2 e2 ...) ...)
     (let ((accept-headers (header-values 'accept (request-headers request))))
       (cond ((pair? (lset-intersection eq? accept-headers 't1)) e1 ...)
             ((pair? (lset-intersection eq? accept-headers 't2)) e2 ...)
             ...
             (else (make-awful-response code: 406 reason: "Not Acceptable")))))))

(define (define-login-trampoline path #!key vhost-root-path hook)
  (define-page path
    (lambda ()
      (let* ((user ($ 'user))
             (password ($ 'password))
             (attempted-path ($ 'attempted-path))
             (password-valid? ((valid-password?) user password))
             (new-sid (and password-valid? (session-create))))
        (sid new-sid)
        (when (enable-session-cookie)
          ((session-cookie-setter) new-sid))
        (when hook (hook user))
        (html-page
         ""
         headers: (<meta> http-equiv: "refresh"
                          content: (++ "0;url=" (or attempted-path (main-page-path)))))))
    method: 'POST
    vhost-root-path: vhost-root-path
    no-session: #t
    no-template: #t))

(valid-password? 
  (lambda (user password) #t))

(define (access-control path)
  (cond ((member path `(,(login-page-path) "/login-trampoline")) ;; allow access to login-related pages
         #t)
        (else
          (let ((user (read-cookie 'user)))
            (and user (string? user))))))

(define (pox base-path database-credentials #!key (awful-settings (lambda (_) (_))))

  (define base-path-pattern (irregex (string-append (string-chomp base-path "/") "(/.*)*")))

  (define-app pox
							matcher: (lambda (path)
												 (irregex-match base-path-pattern path))
							handler-hook: (lambda (handler)
															(parameterize ((enable-sxml #t)
                                             (enable-ajax #t)
                                             (app-root-path base-path)
                                             (db-credentials database-credentials)
                                             (enable-session-cookie #t)
                                             (session-cookie-name "pox-sid")
                                             (page-charset "utf-8")
                                             (page-access-control access-control))
																						(switch-to-postgresql-database)
																						(awful-settings handler)))
              (define-page (login-page-path)
                (lambda ()
                  (login-form submit-label: "Sign in"))
                no-session: #t)

              (define-page "/logout"
                (lambda ()
                  (set-cookie! 'user)))

              (define-login-trampoline "/login-trampoline" hook: (lambda (user)
                                                                   (set-cookie! 'user user)))

              (define-page (main-page-path)
                (lambda ()
                  "")
                headers: (include-javascript "/js/jquery.js" "/js/jquery.query.js" "/js/jquery.cookie.js" "/js/app.js")
                css: '("/css/layout.css" "/css/main.css")
                title: "pox")

              (define-page "tasks"
                (lambda ()
                  (let ((user (read-cookie 'user)))
                    (parameterize ((user-map (select-users)))
                                  (if user
                                    (task-response (get-tasks user))
                                    (task-response (get-tasks)))))))

                (define-page (irregex "/users/(.*)/tasks")
                  (lambda (path)
                    (let ((name (irregex-match-substring (irregex-search (irregex "/users/(.*)/tasks") path) 1))
                          (user (read-cookie 'user))
                          (method (request-method (current-request))))
                      (parameterize ((user-map (select-users)))
                                    (if (eq? method 'GET)
                                      (task-response (get-tasks user))
                                      (or (and-let* ((headers (request-headers (current-request)))
                                                     (content-length (header-value 'content-length headers))
                                                     (body (read-string content-length (request-port (current-request))))
                                                     (accept (header-values 'accept (request-headers (current-request))))
                                                     (tasks (http-accept-case 
                                                              (current-request)
                                                              ((application/json)
                                                               ;; the null list is the preamble; 
                                                               ;; should add that to JSON as well, I guess
                                                               (cons '() (read-json-tasks body)))
                                                              ((text/x-downtime)
                                                               (cdr (with-input-from-string body downtime-read)))))
                                                     (_ (unless (pair? (cdr tasks)) (print "exit!!!"))) ;; FIXME: not very pretty
                                                     (conflicts (persist-user-tasks (string->user-id user) tasks)))
                                            (if (null? conflicts)
                                              (make-awful-response status: 'no-content)
                                              (make-awful-response status: 'conflict
                                                             body: (conflicts->string conflicts (string->user-name user)))))
                                          (make-awful-response status: 'internal-server-error
                                                         body: "Error handling input data"))))))
                  title: "pox"
                  method: '(GET POST))))

(define (task-page user tasks)
  (let ((title (if user `("Tasks of " ,user) "Tasks")))
    (set-page-title! title)
    (<body>
      (<div> data-role: "page" id: "tasks"
             (<div> data-role: "header"
                    class: "ui-bar"
                    (<a> href: "#login" data-icon: "back" "Change user")
                    (<h1> title))
             (<div> data-role: "content"
                    (<div> id: "tasks" data-role: "collapsible-set"
                           (map (lambda (t)
                                  (let ((id (number->string (alist-ref 'id t))))
                                    (<div> data-role: "collapsible"
                                           (<h3> (alist-ref 'name t))
                                           (and-let* ((desc (alist-ref 'description t)))
                                             (simple-format desc))
                                           (<div> data-role: "navbar"
                                                  (<ul> (<li> 
                                                          (<a> href: (++ "/tasks/" id "/edit")
                                                               data-iconpos: "right"
                                                               data-icon: "gear"
                                                               "Edit"))
                                                        (<li> 
                                                          (<a> href: (++ "/tasks" id "/delegate")
                                                               data-iconpos: "right"
                                                               data-icon: "forward"
                                                               "Delegate"))
                                                        (<li> 
                                                          (<a> href: (++ "/tasks" id "/done")
                                                               data-iconpos: "right"
                                                               data-icon: "check"
                                                               "Done")))))))
                                tasks)))))))
                  
(define (get-tasks #!optional user)
  (with-request-variables 
    ((group-by as-grouping)
     (filter as-filter)
     (ignore as-list-of-symbols)
     (include-done as-boolean)
     (skip as-list-of-symbols))
    (if user
      (let* ((user (string->user-name user))
             (tasks (select-user-tasks user group-by filter include-done)))
        (if (or (not tasks) (null? tasks))
          (and (db-select-one 'users 'name user 'id) '())
          tasks))
      (select-tasks group-by filter include-done))))

(define (task-response tasks #!optional user)
  (let ((request (current-request)))
    (with-request-variables 
      ((ignore as-list-of-symbols)
       (skip as-list-of-symbols))
      (if (not tasks)
        (make-awful-response status: 'not-found body: "Not Found")
        (http-accept-case request
                          ((application/json)
                           (send-json-response (task-list->json-serializable tasks)))
                          ((text/x-downtime)
                           (send-downtime-response (task-list->string tasks user request ignore skip)))
                          ((text/html)
                           (task-page user tasks)))))))
