(ns is.simm.views.tasks
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            #?(:clj [datahike.api :as d])))

;; Saving this file will automatically recompile and update in your browser

(def task-cfg {:store {:backend :mem :id "task-manager"}})

(def task-schema [{:db/ident :title
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :start
                   :db/valueType :db.type/instant
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :end
                   :db/valueType :db.type/instant
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :children
                   :db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/many}])

#?(:clj
   (defn init-db [cfg schema]
     (try
       (d/create-database cfg)
       (let [conn (d/connect cfg)]
         (d/transact conn schema))
       (catch Exception e
         (println "Database already exists")))))

#?(:clj (init-db task-cfg task-schema))

#?(:clj (defonce task-conn (d/connect task-cfg)))

(comment
  (d/delete-database cfg)

  (d/transact task-conn [{:title "Nachhaltiges Mannheim 2014"
                          :start #inst "2014-01-01T00:00:00.000-00:00"
                          :end #inst "2014-12-31T23:59:59.999-00:00"
                          :children [{:title "Urban gardening"
                                      :start #inst "2014-01-01T00:00:00.000-00:00"
                                      :end #inst "2014-12-31T23:59:59.999-00:00"
                                      :children [{:title "Gartenprojekt"
                                                  :start #inst "2014-01-01T00:00:00.000-00:00"
                                                  :end #inst "2014-12-31T23:59:59.999-00:00"}]}]}])

  (d/datoms @task-conn :eavt)

  (d/transact task-conn schema)

  (d/q '[:find ?t ?ct
         :where
         [?e :title ?t]
         [?e :children ?c]
         [?c :title ?ct]]
    @task-conn))

;; a user management system

;; schema containing user, password, and role

(e/defn QueryParentTimeline [title]
  (e/server (d/q '[:find ?title ?start ?end
                   :in $ ?parent-title
                   :where
                   [?t :title ?parent-title]
                   [?pt :children ?t]
                   [?pt :title ?title]
                   [?pt :start ?start]
                   [?pt :end ?end]]
              (e/watch task-conn)
              title)))

(e/defn QueryTimeline [title]
  (e/server (d/q '[:find ?title ?start ?end
                   :in $ ?title
                   :where
                   [?t :title ?title]
                   [?t :start ?start]
                   [?t :end ?end]]
              (e/watch task-conn)
              title)))

(e/defn QueryChildrenTimeline [parent-title]
  (e/server (d/q '[:find ?title ?start ?end
                   :in $ ?parent-title
                   :where
                   [?pt :title ?parent-title]
                   [?pt :children ?t]
                   [?t :title ?title]
                   [?t :start ?start]
                   [?t :end ?end]]
              (e/watch task-conn)
              parent-title)))

(e/defn Task [active-task text start end]
  (e/client
    (dom/div (dom/props {:class "parent" :style {:opacity (if (empty? text) "0.5" "1")}})
      (dom/div (dom/props {:class "timeline"})
        (dom/div (dom/props {:class "timeline-left"})
          (dom/img (dom/props {:src "parent-left.png"
                               :style {:height "30px"}
                               :alt (str start)})))
        (dom/div (dom/props {:class "parent-timeline-text"})
          (dom/text text)
          (dom/On-all "click" (e/fn [e]
                            (.log js/console "click" e)
                            (reset! active-task text))))
        (dom/div (dom/props {:class "timeline-right"})
          (dom/img (dom/props {:src "parent-right.png"
                               :style {:height "30px"}
                               :alt (str end)})))))))

#?(:cljs
   (def !active-task (atom "Urban gardening")))

(e/defn TaskManager []
  (let [active-task-title (e/client (e/watch !active-task))
        [parent-title parent-start parent-end] (first (QueryParentTimeline active-task-title))
        [title start end] (first (QueryTimeline active-task-title))]
    (e/client (dom/div (dom/props {:class "flex flex-col items-center justify-center"})
                (dom/div (dom/props {:class "container mx-auto text-center"})
                  (Task !active-task parent-title parent-start parent-end)
                  (Task !active-task title start end)
                  (dom/div (dom/props {:class "container w-11/12"})
                    (e/for [[title start end] (QueryChildrenTimeline title)]
                      (Task !active-task title start end))))))))
