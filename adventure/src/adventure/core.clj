(ns adventure.core
  (:gen-class))
(require '[clojure.string :as str])

(def init-map
  {:foyer {:desc "This is the beginning of the test. "
           :title "in the starting room.  What is needed is not here"
           :dir {:north :room-1
                 :east :room-2
                 :south :room-3
                 :west :room-4}
           :contents :8}

   :room-1 {:desc "It is a large room with the number 1 marked on the ceiling. "
            :title "in Room 1"
            :dir {:east :room-6
                  :south :foyer
                  :west :room-5}
            :contents :1}
   
   :room-2 {:desc "It is a large room with the number 2 marked on the ceiling. "
            :title "in Room 2"
            :dir {:north :room-6
                  :south :room-7
                  :west :foyer}
            :contents :2}
   
   :room-3 {:desc "It is a large room with the number 3 marked on the ceiling. "
            :title "in Room 3"
            :dir {:north :foyer
                  :east :room-7
                  :west :room-9}
            :contents :3}
   
   :room-4 {:desc "It is a large room with the number 4 marked on the ceiling. "
            :title "in Room 4"
            :dir {:north :room-5
                  :east :foyer
                  :south :room-9}
            :contents :4}

   :room-5 {:desc "It is a large room with the number 5 marked on the ceiling. "
            :title "in Room 5"
            :dir {:east :room-1
                  :south :room-4}
            :contents :5}
   
   :room-6 {:desc "It is a large room with the number 6 marked on the ceiling. "
            :title "in Room 6"
            :dir {:south :room-2
                  :west :room-1}
            :contents :6}
   
   :room-7 {:desc "It is a large room with the number 7 marked on the ceiling. "
            :title "in Room 7"
            :dir {:north :room-2
                  :west :room-3}
            :contents :7}  

   :room-9 {:desc "It is a large room with the number 9 marked on the ceiling. "
            :title "in Room 9"
            :dir {:north :room-9
                  :east :room-3}
            :contents :9}
   })

(def init-items
  {:1 {:desc "The number one. It's not the solution."
       :name "The number one."}
   
   :2 {:desc "The number two. It's not the solution."
       :name "The number two."}
   
   :3 {:desc "The number three. It's not the solution."
       :name "The number three."}
   
   :4 {:desc "The number four. It's not the solution."
       :name "The number four."}
   
   :5 {:desc "The number five. It's not the solution."
       :name "The number five."}
   
   :6 {:desc "The number six. It's not the solution."
       :name "The number six."}
   
   :7 {:desc "The number seven. It's not the solution."
       :name "The number seven."}
   
   :8 {:desc "The number eight, the solution to the puzzle."
       :name "The number eight."
       :transform :win-game}
   
   :9 {:desc "The number nine. It's not the solution."
       :name "The number nine."}
   
   :win-game {:desc "Hooray, you won the game"
              :name "Winner"}})

(def init-adventurer
  {:location :foyer
   :inventory #{}
   :hp 10
   :lives 3
   :tick 0
   :seen #{}})

(defn status [state]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((get-in state [:adventurer :seen]) location)
      (print (-> the-map location :desc)))
    (update-in state [:adventurer :seen] #(conj % location))))

(defn go [state dir]
  (let [location (get-in state [:adventurer :location])
        dest (get-in state [:map location :dir (keyword dir)])]
    (if (nil? dest)
      (do (println "You can't go that way.") state)
      (assoc-in state [:adventurer :location] dest))))

(defn look [state object]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)
        temp (get-in state [:items object])]
    (if (= (name object) (name (-> the-map location :contents)))
          (do (println (str (get-in temp [:name]) ".  "  (get-in temp [:desc]))) state)
          (do (println "What you're looking for doesn't exist.") state))))

(defn look-around [state room]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)
        temp (get-in state [:map room])]
    (if (= (name room) (name location))
      (do (println (str (name (get-in state [:map (get-in state [:adventurer :location]) :contents (get-in state [:map :contents]) :desc])))) state)
      (do (println "You are not in this room.") state))))

(defn inventory [state player]
  (let [inv (get-in state [:adventurer :inventory])]
  (if (empty? inv)(do (println "Your inventory is empty.") state)
    (do (println (str "Your most recent item: " (name (last inv)))) state))))

(defn grab [state object]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)
        item (get-in state [:map location :contents])]
    (if (= (name object) (name (get-in state [:map location :contents])))
      (do (println "You took" (name object)) (update-in state [:adventurer :inventory] #(conj % item)))
      (do (println "There isn't a" (name object) "here.") state))))

(defn drop-item [state object]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)]
    (if (empty? (get-in state [:adventurer :inventory]))
    (do (println "You don't have a" (name object) ".") state)
    (do (println "You attempted to drop" (name object) ", but it refuses to leave.") state))))


(defn quit [state quit]
  (System/exit 0))

(defn solve [state object]
  (if (= (name object) "8")
    (do (println "Congragulations, you solved the puzzle!") (System/exit 0))
    (do (println "That item can't be transformed.") state)))

(defn canonicalize
  "Given an input string, strip out whitespaces, lowercase all words, and convert to a vector of keywords."
  [input]
  (loop [string (str/split (str/replace (str/lower-case input) #"[?.!]" "") #" +")
         final []]
    (if (empty? string) (flatten [final])
        (recur (rest string)
               (conj final (keyword (first string)))))))

(defn match [pattern input]
  (loop [pattern pattern
         input input
         vars '()]
    (cond (and (empty? pattern) (empty? input))
          (reverse vars)

          (or (empty? pattern) (empty? input))
          nil

          (= (first pattern) "@")
          (recur (rest pattern)
                 (rest input)
                 (cons (first input) vars))

          (= (first pattern) (first input))
          (recur (rest pattern)
                 (rest input)
                 vars)

          :else nil)))

(def bank [[:go "@"] go
           [:look :at "@"] look
           [:look :around "@"] look-around
           [:look "@"] look
           [:take "@"] grab
           [:drop "@"] drop-item
           ["@" :inventory] inventory
           [:quit "@"] quit
           [:offer "@"] solve])

(defn respond 
  [state command]
  (loop [bank bank
         command command
         state state 
         index 0]
    (cond (> index 17) 
          (do (println "I don't know what you mean.") state)
          (not (= (match (nth bank index) command) nil))
          ;1 argument
          (cond (or (= index 0) (= index 2) (= index 4) (= index 6) (= index 8) (= index 10) (= index 12) (= index 14) (= index 16))
                ((nth bank (+ index 1)) state 
                                        (nth (match (nth bank index) command) 0)))
          (= (match (nth bank index) command) nil)
          (recur bank
                 command
                 state
                 (+ index 2)))))

(defn -main
  "Initialize the adventure"
  [& args]
  (loop [local-state {:map init-map :adventurer init-adventurer :items init-items}
         ]
    (let [pl (status local-state)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur (respond pl (canonicalize command))))))
