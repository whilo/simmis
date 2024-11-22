(ns is.simm.main
  #?(:cljs (:require-macros [is.simm.main :refer [with-reagent]]))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-svg :as svg]
            #?(:clj [datahike.api :as d])
            #?(:cljs [reagent.core :as r])
            [missionary.core :as m]
            [clojure.string :as str]
            #?(:cljs [reagent.core :as r])
            #?(:cljs ["recharts" :refer  [ScatterChart Scatter LineChart Line
                                          XAxis YAxis CartesianGrid]])
            #?(:cljs ["react-dom/client" :as ReactDom])))



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

(def user-cfg {:store {:backend :mem :id "user-manager"}})

(def user-schema [{:db/ident :user
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :password
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :role
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}])

#?(:clj
   (defn init-db [cfg schema]
     (try
       (d/create-database cfg)
       (defonce conn (d/connect cfg))
       (d/transact conn schema)
       (catch Exception e
         (println "Database already exists")
         (defonce conn (d/connect cfg))))))

#?(:clj
   (do (init-db task-cfg task-schema)
     (init-db user-cfg user-schema)))

(comment
  (d/delete-database cfg)

  (d/transact conn [{:age 42 :name "Alice"}])

  (d/transact conn [{:age 42 :name "Beatrix"}])

  (d/transact conn [{:title "Nachhaltiges Mannheim 2014"
                     :start #inst "2014-01-01T00:00:00.000-00:00"
                     :end #inst "2014-12-31T23:59:59.999-00:00"
                     :children [{:title "Urban gardening"
                                 :start #inst "2014-01-01T00:00:00.000-00:00"
                                 :end #inst "2014-12-31T23:59:59.999-00:00"
                                 :children [{:title "Gartenprojekt"
                                             :start #inst "2014-01-01T00:00:00.000-00:00"
                                             :end #inst "2014-12-31T23:59:59.999-00:00"}]}]}])

  (d/datoms @conn :eavt)

  (d/transact conn schema)

  (d/q '[:find ?t ?ct
         :where
         [?e :title ?t]
         [?e :children ?c]
         [?c :title ?ct]]
    @conn))

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
              (e/watch conn)
              title)))

(e/defn QueryTimeline [title]
  (e/server (d/q '[:find ?title ?start ?end
                   :in $ ?title
                   :where
                   [?t :title ?title]
                   [?t :start ?start]
                   [?t :end ?end]]
              (e/watch conn)
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
              (e/watch conn)
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
          (dom/on "click" (e/fn [e]
                            (.log js/console "click" e)
                            (reset! active-task text))))
        (dom/div (dom/props {:class "timeline-right"})
          (dom/img (dom/props {:src "parent-right.png"
                               :style {:height "30px"}
                               :alt (str end)})))))))

#?(:cljs
   (def !view-state (atom :main)))

#?(:cljs 
   (def !user (atom nil)))

(e/defn Header []
  (e/client
    (dom/header (dom/props {:class "bg-gray-600 p-4 shadow"})
      (dom/div (dom/props {:class "container mx-auto flex justify-between items-center"})
        (dom/div (dom/props {:class "flex items-center"})
          (dom/a (dom/props {:class "text-white text-xl font-bold"})
            (dom/on "click" (e/fn [_e] (reset! !view-state :main)))
            (dom/img (dom/props {:src "/simmis.png" :alt "Logo" :class "h-8"}))))
        (dom/nav (dom/props {:class "flex space-x-4"})
          (dom/a (dom/props {:class "text-white" :href "#about"})
            (dom/on "click" (e/fn [_e] (reset! !view-state :about)))
            (dom/text "About"))
          (dom/a (dom/props {:class "text-white" :href "#taskmanager"})
            (dom/on "click" (e/fn [_e] (reset! !view-state :taskmanager)))
            (dom/text "Task Manager"))
          (dom/a (dom/props {:class "text-white" :href "#screenshare"})
            (dom/on "click" (e/fn [_e] (reset! !view-state :screenshare)))
            (dom/text "Screen Share"))
          (if-let [user (e/watch !user)]
            (dom/a (dom/props {:class "text-white" :href "#user"})
              (dom/on "click" (e/fn [_e] (js/alert "not implemented")))
              (dom/text (:user user)))
            (dom/a (dom/props {:class "text-white" :href "#login"})
              (dom/on "click" (e/fn [_e] (reset! !view-state :login)))
              (dom/text "Login"))))))))


(e/defn Footer []
  (e/client
    (dom/footer (dom/props {:class "bg-gray-200 p-4"})
      (dom/div (dom/props {:class "container mx-auto flex justify-between text-center"})
        (dom/p (dom/text "Powered by ")
          (dom/a (dom/props {:href "https://clojure.org/" :target "_blank" :class "text-blue-400"})
            (dom/text "Clojure"))
          (dom/text ", ")
          (dom/a (dom/props {:href "https://datahike.io" :target "_blank" :class "text-blue-400"})
            (dom/text "Datahike"))
          (dom/text " and ")
          (dom/a (dom/props {:href "https://hyperfiddle.net/" :target "_blank" :class "text-blue-400"})
            (dom/text "Electric")))))))

#?(:cljs
   (def !active-task (atom "Urban gardening")))

(e/defn TaskManager []
  (let [active-task-title (e/client (e/watch !active-task))
        [parent-title parent-start parent-end] (first (QueryParentTimeline. active-task-title))
        [title start end] (first (QueryTimeline. active-task-title))]
    (e/client (dom/div (dom/props {:class "flex flex-col items-center justify-center"})
                (dom/div (dom/props {:class "container mx-auto text-center"})
                  (Task. !active-task parent-title parent-start parent-end)
                  (Task. !active-task title start end)
                  (dom/div (dom/props {:class "container w-11/12"})
                    (e/for [[title start end] (QueryChildrenTimeline. title)]
                      (Task. !active-task title start end))))))))

(e/defn ScreenShare []
  (e/client
    (dom/div (dom/props {:class "flex flex-col items-center justify-center bg-gray-100"})
      (dom/div (dom/props {:class "max-w-2xl p-6 bg-white rounded-lg shadow-md text-center"})
        (dom/h1 (dom/props {:class "text-4xl font-bold mb-4 text-gray-800"}) (dom/text "ScreenShare"))
        (dom/p (dom/props {:class "text-lg font-medium text-gray-700 mb-2"})
          (dom/text "This is a simple screen sharing application using Electric."))
        (dom/p (dom/props {:class "text-gray-600"})
          (dom/text "It demonstrates how to use Electric to build a simple web application."))))))


(e/defn CheckLogin? [user password]
  (e/server
    (prn user password)
    true))

(e/defn Login []
  (e/client
    (let [submit-fn (e/fn [e]
                      (.preventDefault e)
                      (let [user (.-value (.getElementById js/document "user"))
                            password (.-value (.getElementById js/document "password"))]
                        (if (CheckLogin?. user password)
                          (do
                            (reset! !user {:user user :password password})
                            (reset! !view-state :main))
                          (do
                            (reset! !user nil)
                            (js/alert "Invalid username or password.")))))]
      (dom/div (dom/props {:class "flex flex-col items-center justify-center"})
        (dom/div (dom/props {:class "w-full max-w-md"})
          (dom/h1 (dom/props {:class "text-3xl font-bold mb-6"}) (dom/text "Signup"))
          (dom/form (dom/props {:class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4"})))
        (dom/on "submit" submit-fn)
        (dom/div (dom/props {:class "mb-4"})
          (dom/label (dom/props {:class "block text-gray-700 text-sm font-bold mb-2"} (dom/text "Username")))
          (dom/input (dom/props {:id "user" :class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" :type "text" :placeholder "Username"})))
        (dom/div (dom/props {:class "mb-6"})
          (dom/label (dom/props {:class "block text-gray-700 text-sm font-bold mb-2"} (dom/text "Password")))
          (dom/input (dom/props {:id "password" :class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline" :type "password" :placeholder "Password"})))
        (dom/div (dom/props {:class "flex items-center justify-between"})
          (dom/button (dom/props {:class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline" :type "submit"})
            (dom/on "click" submit-fn)
            (dom/text "Login")))))))

(e/defn About []
  (e/client
    (dom/section (dom/props {:class "flex flex-col items-center justify-center bg-gray-100 grow"})
      (dom/div (dom/props {:class "max-w-2xl p-6 bg-white rounded-lg shadow-md"})
        (dom/h1 (dom/props {:class "text-4xl font-bold mb-4 text-center text-gray-800"}) (dom/text "About"))
        (dom/p (dom/props {:class "text-lg font-medium text-gray-700 mb-2 text-center"})
          (dom/text "This is a simple task manager application using Electric."))
        (dom/p (dom/props {:class "text-gray-600 text-center"})
          (dom/text "It demonstrates how to use Electric to build a simple web application."))))))



#?(:cljs (def ReactRootWrapper
           (r/create-class
            {:component-did-mount (fn [this] (js/console.log "mounted"))
             :render (fn [this]
                       (let [[_ Component & args] (r/argv this)]
                         (into [Component] args)))})))

#?(:cljs (defn create-root
           "See https://reactjs.org/docs/react-dom-client.html#createroot"
           ([node] (create-root node (str (gensym))))
           ([node id-prefix]
            (ReactDom/createRoot node #js {:identifierPrefix id-prefix}))))

#?(:cljs (defn render [root & args] 
           (.render root (r/as-element (into [ReactRootWrapper] args)))))

(defmacro with-reagent [& args]
  `(dom/div  ; React will hijack this element and empty it.
     (let [root# (create-root dom/node)]
       (render root# ~@args)
       (e/on-unmount #(.unmount root#)))))

;; Reagent World

(defn TinyLineChart [data]
  #?(:cljs
     [:> LineChart {:width 400 :height 200 :data (clj->js data)}
      [:> CartesianGrid {:strokeDasharray "3 3"}]
      [:> XAxis {:dataKey "name"}]
      [:> YAxis]
      [:> Line {:type "monotone", :dataKey "pv", :stroke "#8884d8"}]
      [:> Line {:type "monotone", :dataKey "uv", :stroke "#82ca9d"}]]))

(defn MousePosition [x y]
  #?(:cljs
     [:> ScatterChart {:width 300 :height 300 
                       :margin #js{:top 20, :right 20, :bottom 20, :left 20}}
      [:> CartesianGrid {:strokeDasharray "3 3"}]
      [:> XAxis {:type "number", :dataKey "x", :unit "px", :domain #js[0 2000]}]
      [:> YAxis {:type "number", :dataKey "y", :unit "px", :domain #js[0 2000]}]
      [:> Scatter {:name "Mouse position",
                   :data (clj->js [{:x x, :y y}]), :fill "#8884d8"}]]))

;; Electric Clojure

(e/defn ReagentInterop []
  (e/client
    (let [[x y] (dom/on! js/document "mousemove"
                         (fn [e] [(.-clientX e) (.-clientY e)]))]
      (with-reagent MousePosition x y) ; reactive
      ;; Adapted from https://recharts.org/en-US/examples/TinyLineChart
      (with-reagent TinyLineChart 
        [{:name "Page A" :uv 4000 :amt 2400 :pv 2400}
         {:name "Page B" :uv 3000 :amt 2210 :pv 1398}
         {:name "Page C" :uv 2000 :amt 2290 :pv (+ 6000 (* -5 y))} ; reactive
         {:name "Page D" :uv 2780 :amt 2000 :pv 3908}
         {:name "Page E" :uv 1890 :amt 2181 :pv 4800}
         {:name "Page F" :uv 2390 :amt 2500 :pv 3800}
         {:name "Page G" :uv 3490 :amt 2100 :pv 4300}]))))

(e/defn Main [ring-request]
  (let [view-state (e/client (e/watch !view-state))]
    (e/client
      (binding [dom/node js/document.body]
        (dom/section (dom/props {:class "flex flex-col min-h-screen"})
          (Header.)
          (dom/div (dom/props {:class "flex-grow"})
            (ReagentInterop.)
            #_(case view-state
                :main (About.)
                :screenshare (ScreenShare.)
                :taskmanager (TaskManager.)
                :login (Login.)
                :about (About.)))
          (Footer.))))))


(comment

  (require #_[app.config :as config]
    'clojure.edn
    'contrib.ednish
    '[contrib.str :refer [any-matches?]]
    '[contrib.data :refer [unqualify treelister]]
    ;;#?(:clj '[contrib.datomic-contrib :as dx])
    '[contrib.datomic-m #?(:clj :as :cljs :as-alias) d]
    '[contrib.gridsheet :as gridsheet :refer [Explorer]]
    '[hyperfiddle.electric :as e]
    '[hyperfiddle.electric-dom2 :as dom]
    '[hyperfiddle.history :as history]
    '[missionary.core :as m])


  )