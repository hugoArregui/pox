(load-relative "../downtime.scm")
(import downtime)

(use test srfi-13)

(require-library regex)
(import irregex)

(define-syntax test-read
  (syntax-rules ()
    ((_ result text) 
     (test-read result '() text))
    ((_ result preamble text)
     (test (let ((t (irregex-replace/all '(+ (or space #\newline)) text " ")))
	     (if (> (string-length t) 40) (substring t 0 40) t))
           `(downtime ,preamble . ,result)
           (with-input-from-string text downtime-read)))))

(define-syntax test-read-error
  (syntax-rules ()
    ((_ error args text)
     (let ((e #f))
       (handle-exceptions exn
         (set! e (list (get-condition-property exn 'exn 'message)
                       (get-condition-property exn 'exn 'arguments)))
         (with-input-from-string text downtime-read))

       (test error e (list error args))))))

(define-syntax test-write
  (syntax-rules ()
    ((_ result user tasks)
     (test-write result user '() tasks))
    ((_ result user* preamble tasks)
     (test (irregex-replace/all '(+ space) result " ")
           result
           (with-output-to-string 
               (lambda ()
                 (downtime-write `(downtime
                                   ((user . ,user*) . ,preamble)
                                   . ,tasks)
                                 '(user))))))))


(test-read '(((name . "foo"))) "* foo")
(test-read '(((name . "foo")) ((name . "bar"))) "* foo\n* bar")

(test-read '(((name . "some task name") (id . 10))
	     ((name . "another one bites the task") (id . 123))
	     ((name . "veryshort")))
	   "*   some task name #10  \n*another one bites the task  #123\n*veryshort#")

(test-read '(((assignee . "daniel") (name . "a nice task"))
	     ((assignee . "bar") (name . "foo")))
	   "* a nice task # > daniel\n*foo#>bar")

(test-read '(((assigner . "somebody") (name . "something") (id . 1)))
	   "* something #1 < somebody   \n   ")

(test-read '(((priority . +4) (name . "important"))
	     ((priority . -2) (name . "irrelevant")))

	   "* important # +4\n*irrelevant#-2")

(test-read '(((priority . 1) (assignee . "bar") (name . "foo")))
	   "* foo # > bar  +1")

(test-read-error "invalid meta data" '("+22") "*foo#+22")

(test-read '(((name . "foo"))) "* foo\n\n")
(test-read-error "invalid meta data" '(">bla x") "* foo #1\n*bar#>bla x")
(test-read-error "invalid meta data" '("bla>blubb") "* foo#bla>blubb")

(test-read '(((description . "see, this is\nthe description") (assigner . "wtf") (name . "read below")))
	   "
* read below # < wtf
  see, this is
  the description")

(test-read '(((description . "** bar") (name . "foo")))
	   "
* foo
** bar")

(test-read '(((description . "bar") (name . "first") (assignee . "foo"))
	     ((name . "second") (assignee . "foo")))
	   "
# > foo
* first
bar
*second")

(test-read '(((name . "some") (id . 1) (assignee . "foo"))
	     ((name . "another") (id . 2) (assigner . "bar") (assignee . "foo"))
	     ((name . "one") (assigner . "bar") (assignee . "foo"))
	     ((name . "one more") (assigner . "baz") (assignee . "foo"))
	     ((name . "well") (assigner . "baz") (assignee . "qux"))
	     ((name . "then") (assigner . "quux")))
	   "
# > foo
* some #1
## < bar
* another #2
* one 
## < baz
* one more
### > qux
* well
# < quux
* then")

(test-read '(((name . "first") (assignee . "foo"))
	     ((name . "second") (assignee . "baz")))
	   "
# > foo
* first
* second # > baz")

(test-read-error "illegal nesting" '("### > bar") "# > foo\n### > bar")

(test-read '(((description . " some\nmulti\n line\n  description") (name . "foo")))
	   "* foo
  some
 multi
  line
   description")

(test-read '(((description . "x\n\n\n\nfoo") (name . "foo")))
	   "* foo
x  
    
 

foo")

(test-read '(((done . #t) (name . "whatever")))
	   "* whatever #done")

(test-read '(((name . "noooo") (done . #t))
	     ((name . "wtf") (done . #f))
	     ((name . "baz") (done . #f))
	     ((name . "ftw") (done . #t)))
	   "
# done
* noooo
* wtf # to do

# to do
* baz
* ftw # done")

(test-read-error "description before item" '("foo")
		 "foo
* bar")

(test-read-error "description before item" '("baz")
		 "* foo
# > bar
baz")

(test-read '(((name . "some task") (revision . 2) (id . 10)))
	   "* some task #10:2")

(test-read-error "invalid meta data" '(".x")
		 "* some task #10.x")


(test-read '(((category . "/bar") (name . "foo"))) "* foo # /bar")


(test-read '(((name . "baz") (category . "/foo/bar"))
	     ((name . "quux") (category . "/foo/qux"))
	     ((category . "/absolute/fubar") (name . "whoa"))) "
# /foo
* baz # /bar

## /qux
* quux
* whoa # //absolute/fubar")

(test-read '(((tags "baz" "bar") (name . "foo"))) "
* foo # :bar :baz
")

(test-write "* bar #10  \n\n" "foo" '(((id . 10) (creator . "foo") (name . "bar"))))

(test-write "* foo #1:2  \n* bar #2 < baz  \n\n" "foo"
	    '(((id . 1) (revision . 2) (name . "foo"))
	      ((id . 2) (revision . 1) (assigner . "baz") (assignee . "foo") (name . "bar"))))

(test-write "* foo #1  \n  bar  \n    \n  baz\n\n\n" "foo"
	    '(((id . 1) (name . "foo") (description . "bar\n\nbaz"))))

(test-write "* foo #1 done  \n\n" "foo"
	    '(((id . 1) (name . "foo") (done . #t))))

(test-write "# > foo
* foo #1  
* bar #2  \n\n" "foo"
'(group assignee
	("foo"
	 ((id . 1) (name . "foo") (assignee . "foo") (assigner . "foo"))
	 ((id . 2) (name . "bar") (assignee . "foo") (assigner . "foo")))))
(test-write "# > foo
## < bar
* baz #1  \n\n" "foo"
'(group assignee
	("foo"
	 group assigner ("bar" 
			 ((id . 1) (name . "baz") (assignee . "foo") (assigner . "bar"))))))

(test-write "* foo #1 < yeah  \n\n" "foo" '(((id . 1) (creator . "foo") (name . "foo") (assigner . "yeah"))))
(test-write "* foo #1 /foo  \n\n" "foo" '(((id . 1) (name . "foo") (category . "/foo"))))

(test-write "* foo #2 < bar  \n\n" #f
            '(((id . 2) (name . "foo") (assigner . "bar"))))

(test-group "creator = A, assignee = assigner = B"
  (test-write "* check #1 < bar > bar  \n\n"
              "foo"
              '(((id . 1)
                 (name . "check")
                 (creator . "foo")
                 (assigner . "bar")
                 (assignee . "bar"))))

  (test-write "# > bar\n* check #1 < bar  \n\n"
              "foo"
              '(group assignee
                      ("bar"
                       ((id . 1)
                        (name . "check")
                        (creator . "foo")
                        (assigner . "bar")
                        (assignee . "bar")))))

  (test-write "# < bar\n* check #1 > bar  \n\n"
              "foo"
              '(group assigner
                      ("bar"
                       ((id . 1)
                        (name . "check")
                        (creator . "foo")
                        (assigner . "bar")
                        (assignee . "bar"))))))

(test-group "creator = A, assignee = B, assigner = C"
  (test-write "* check #1 < bar > baz  \n\n"
              "foo"
              '(((id . 1)
                 (name . "check")
                 (creator . "foo")
                 (assigner . "bar")
                 (assignee . "baz")))))

(test-group "commands"
  (test-group "properties"
    (test-read '(((priority . 2) (assigner . "me") (name . "hey")))
               "* hey # @assigner(me) @priority(2)"))

  (test-group "ignore"
    (test-group "reading"
      (test-read '(((name . "foo") (id . 12))) "
* foo #12 > bar @ignore(description assignee)\n
some descrition that's ignored
")
      (test-read '(((name . "foo")))
                 "
# @ignore(description)
* foo # > hey @ignore(assignee) ...
")
      (test-read-error "invalid task attribute" '(bla)
                       "* hehe # @ignore(bla)"))

    (test-group "writing"
      (test-write "@ignore(assigner)

* foo bar #99  
  check\n\n\n"
                  "foo"
                  '((ignore assigner))
                  '(((id . 99)
                     (name . "foo bar")
                     (description . "check")
                     (assigneer . "bar")
                     (assignee  . "foo"))))

      (test-write "@ignore(description priority)

* nice #1 < what ...  \n\n"
                  "foo"
                  '((ignore description priority))
                  '(((id . 1)
                     (name . "nice")
                     (assigner . "what")
                     (assignee . "foo")
                     (priority . 5)
                     (description . "check it out"))))))
  
  (test-group "invalid"
    (test-read-error "invalid command" '(bar)
                     "* foo # @bar")
    (test-read-error "invalid command syntax" '("@bar(")
                     "* foo # @bar(")))

(test-group "preamble"
  (test-read '(((name . "foo")))
             '((origin . "http://foo/bar.dt") 
               (ignore description))
             "
@origin(\"http://foo/bar.dt\")
@ignore(description)

* foo
bar baz"))

(test-exit)