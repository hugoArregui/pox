(load-relative "../pox.system")
(load-system pox)

(use postgresql ssql ssql-postgresql)
(use pox-db pox-db/helpers)

(define task-query-columns
  '(id created_at revision name description priority done category assignee_id assigner_id creator_id updater_id))

(define task-tags-sub-query
  '(as (coalesce (select (call array_agg tag)
                   (from (as (select (columns tag)
                               (from (as (call unnest (call array_agg tag)) tag)))
                             tags))
                   (where (not (null? tag))))
                 (call cast (as "{}" |text[]|)))
       tags))

(define full-tasks-query
  (let ((columns `(assignee assigner creator updater . ,task-query-columns)))
    `(select (columns tags . ,columns)
       (from
        (as (select (columns ,task-tags-sub-query . ,columns)
              (from
               (as (select (columns (as u1.name assignee)
                                    (as u2.name assigner)
                                    (as u3.name creator)
                                    (as u4.name updater)
                                    (as tags.name tag)
                                    . ,(map (lambda (c) `(col t ,c))
                                            task-query-columns))
                     (from (join left
                                 (join left
                                       (join left
                                             (join left
                                                   (join left
                                                         (join left
                                                               (as tasks t)
                                                               (as users u1)
                                                               (on (= t.assignee_id u1.id)))
                                                         (as users u2)
                                                         (on (= t.assigner_id u2.id)))
                                                   (as users u3)
                                                   (on (= t.creator_id u3.id)))
                                             (as users u4)
                                             (on (= t.updater_id u4.id)))
                                       (as task_tags tt)
                                       (on (= tt.task_id t.id)))
                                 tags
                                 (on (= tt.tag_id tags.id)))))
                   tasks_with_tags))
              (group . ,columns))
            tasks)))))

(unless (db-connection)
  (load-relative "../init")
  (db-connection (connect (db-connection-spec))))

(with-transaction (db-connection)
  (lambda ()
    (db-query "DROP VIEW IF EXISTS tasks_with_tags")
    (db-query (sprintf "CREATE VIEW tasks_with_tags AS ~A"
                       (ssql->sql (db-connection) full-tasks-query)))))
