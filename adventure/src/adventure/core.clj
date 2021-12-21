(ns adventure.core
  (:gen-class))

(def init-map {:bar {:desc "This is a bar where you can relax and have fun with your friends. You can reduce some pressures in this place.",
                     :title "in the bar",
                     :dir {:east "Home"},
                     :contents #{:beer}},
               :home {:desc "This is your home. You can sleep here to restore your energy."
                      :title "in your home"
                      :dir {:west :bar, :east :restaurant}
                      :content #{:bed}},
               :restaurant {:desc "This is a restaurant on green street. You can eat here to increase your food point. You can also order take-out to put food in your backpack so that you can eat it later.",
                            :title "in the restaurant",
                            :dir {:south :altgeld, :west :home, :east :grainger},
                            :content #{:food :take-out}},
               :altgeld {:desc "This is Altgeld Hall. The legends says that there is a mysterious Dairy Queen down in the basement of Altgeld. If you find it, you can get an Illini Blizzard to restore all points.",
                         :title "in Altgeld Hall",
                         :dir {:north :restaurant, :east :starbucks},
                         :content #{:possibly-existed-Dairy-Queen}},
               :starbucks {:desc "This is the starbucks in the Illini Union. You can get some coffee to restore a small portion of your energy."
                           :title "in Starbucks"
                           :dir {:north :grainger, :west :altgeld}
                           :content #{:coffee}},
               :grainger {:desc "This is Grainger Library. You can fetch, work on, and push your MP and take your exam"
                          :title "in Grainger Library"
                          :dir {:north :digital, :west :restaurant, :south :starbucks, :east :isr}
                          :content #{:linuxMachine, :cbtf}},
               :isr {:desc "This is ISR dining hall You can eat here and restore your food point and release your pressure."
                     :title "in ISR"
                     :dir {:west :grainger}
                     :content #{:dining}},
               :digital {:desc "This is Digital Lab. You can fetch, work on, and push your MP here."
                         :title "in Digital Lab"
                         :dir {:north :siebel, :south :grainger}
                         :content #{:linuxMachine}},
               :siebel {:desc "This is Siebel Center for Computer Science. You can attend the honors lecture taught by Prof. Beckman here."
                        :title "in Siebel Center"
                        :dir {:north :eceb, :south :digital}
                        :content #{:beckman}},
               :eceb {:desc "This is ECE Building. You can attend the lecture taught by Prof. Evans."
                      :title "in ECEB"
                      :dir {:south :siebel}
                      :content #{:evans}}})

;; north, south, west, east


(def uiuc {"Bar" ["Empty" "Empty" "Empty" "Home"],
           "Home" ["Empty" "Empty" "Empty" "Restaurant"],
           "Restaurant" ["Empty" "Altgeld Hall" "Home" "Grainger Library"],
           "Altgeld Hall" ["Restaurant" "Empty" "Empty" "Starbucks"],
           "Starbucks" ["Grainger Library" "Altgeld Hall" "Empty" "Empty"],
           "Grainger Library" ["Digital Lab" "Restaurant" "Empty" "ISR"],
           "ISR" ["Empty" "Empty" "Grainger Library" "Empty"],
           "Digital Lab" ["Siebel Center" "Grainger Library" "Empty" "Empty"],
           "Siebel Center" ["ECE Building" "Digital Lab" "Empty" "Empty"],
           "ECE Building" ["Empty" "Siebel Center" "Empty" "Empty"]})
(def uiuc-size (count uiuc))

(defn vector-has [v elt]
  (some #{elt} v))

(defn rand-unique
  "Pick a random number from 0 to `max-1` that is not in the set `exclude`.  Does not check for errors."
  [max exclude]
  (let [pick (rand-int max)]
    (if (exclude pick) (rand-unique max exclude) pick)))

(def init-student
  {:location :home
   :inventory {:mp (int 0), :exam "NON-Taken"}
   :seen #{}
   :understanding (int 0)
   :energy (int 100)
   :food (int 0)
   :pressure (int 50)
   :wellness (int 100)
   :progress (int 0)
   :win (int 0)
   :mp-score "NON-Pushed"
   :honors {:understanding (int 0), :project (int 0)}})

(def init-state {:student init-student, :map init-map})

(defn go [state dir]
  (let [loc (get-in state [:student :location])
        dest ((get-in state [:map loc :dir]) dir)]
    (if (nil? dest)
      (do (println "You can't go that way.") state)
      (do (println "You are going to " dest) (assoc-in state [:student :location] dest)))))

(defn status [state]
  (let [loc (get-in state [:student :location])
        the-map (:map state)
        visited (get-in state [:student :seen])]
    (println (str "You are " (-> loc the-map :title) ". "))
    (when-not ((get-in state [:student :seen]) loc)
      (println (-> the-map loc :desc)))
    (assoc-in state [:student :seen] (conj visited #{loc}))))

(defn building [state]
  (let [loc (get-in state [:student :location])
        pre (get-in state [:student :pressure])
        wel (get-in state [:student :wellness])
        pro (get-in state [:student :progress])
        foo (get-in state [:student :food])
        enr (get-in state [:student :energy])
        und (get-in state [:student :understanding])
        num (get-in state [:student :inventory :mp])]
    (cond (= loc :bar)
          (do (println "You can [D]rink or [P]arty to reduce pressure. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "D")
                      (do (println "You chose to drink")
                          (assoc-in state [:student :pressure] (- pre 50))
                          (assoc-in state [:student :wellness] (- wel 30)))
                      (= choice "P")
                      (do (println "You chose to party")
                          (assoc-in state [:student :pressure] (- pre 5)))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :home)
          (do (println "You can [S]leep to increase energy. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "S")
                      (do (println "You chose to sleep")
                          (assoc-in state [:student :energy] 100))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :restaurant)
          (do (println "You can [E]at to increase your food point. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "E")
                      (do (println "You chose to eat. Food point + 50")
                          (assoc-in state [:student :food] (+ foo 50)))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :altgeld)
          (do (println "You can try to [F]ind the DQ. If you find it, you will win the game! If you can't find it, you will lose the game! Or [L]eave")
              (let [choice (read-line)
                    chance (rand-int 2)]
                (cond (= choice "F")
                      (do (println "You chose to look for DQ")
                          (if (= chance 1) (do (println "CONGRATS! You found the Altgeld DQ!") (assoc-in state [:student :win] 1)) (do (println "Oops! You did not find the Altgeld DQ") (assoc-in state [:student :win] -1))))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :starbucks)
          (do (println "You can [D]rink coffee to increase your energy point. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "D")
                      (do (println "You chose to drink coffee. Energy + 10, Wellness - 5")
                          (assoc-in state [:student :energy] (+ enr 10))
                          (assoc-in state [:student :wellness] (- wel 5)))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :grainger)
          (do (println "You can [F]etch your MP, [W]ork on the MP, [P]ush your MP, or [T]ake the exam. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "F")
                      (if (= num 0) (do (println "You chose to fetch your MP")
                                        (assoc-in state [:student :inventory :mp] 1)) (do (println "You have already fetched your MP!") state))
                      (= choice "W")
                      (if (> num 0) (do (println "You chose to work on the MP!")
                                        (assoc-in state [:student :inventory :mp] (- (+ (get-in state [:student :inventory :mp]) (/ und 10) (/ enr 10) (/ foo 10) (/ wel 5)) (/ pre 5)))
                                        (println "You completed " (-> state :student :inventory :mp) "% of the MP") state)
                          (do (println "You have to fetch your MP first!") state))
                      (= choice "P") (if (> num 0) (do (println "You chose to push the MP!")
                                                       (assoc-in state [:student :mp-score] (-> state :student :inventory :mp))
                                                       (println "you got a " (-> state :student :mp-score) " for the MP!")
                                                       (assoc-in state [:student :progress] (+ 50 pro)))
                                         (do (println "You have to fetch your MP first!") state))
                      (= choice "T") (do (println "You chose to take the exam!")
                                         (assoc-in state [:student :inventory :exam] und)
                                         (println "You got a " und "on the exam!")
                                         (assoc-in state [:student :progress] (+ 50 pro)))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :isr)
          (do (println "You can [E]at at the ISR dining hall to increase your food point and wellness. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "E")
                      (do (println "You chose to eat")
                          (assoc-in state [:student :food] (+ foo 30))
                          (assoc-in state [:student :wellness] (+ wel 10)))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :digital)
          (do (println "You can [F]etch your MP, [W]ork on the MP, or [P]ush your MP. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "F")
                      (if (= num 0) (do (println "You chose to fetch your MP")
                                        (assoc-in state [:student :inventory :mp] 1))
                          (do (println "You have already fetched your MP!") state))
                      (= choice "W")
                      (if (> num 0) (do (println "You chose to work on the MP!")
                                        (assoc-in state [:student :inventory :mp] (- (+ (get-in state [:student :inventory :mp]) (/ und 10) (/ enr 10) (/ foo 10) (/ wel 5)) (/ pre 5)))
                                        (println "You completed " (-> state :student :inventory :mp) "% of the MP") state)
                          (do (println "You have to fetch your MP first!") state))
                      (= choice "P") (if (> num 0) (do (println "You chose to push the MP!")
                                                       (assoc-in state [:student :mp-score] (-> state :student :inventory :mp))
                                                       (println "you got a " (-> state :student :mp-score) " for the MP!")
                                                       (assoc-in state [:student :progress] (+ 50 pro)))
                                         (do (println "You have to fetch your MP first!") state))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :siebel)
          (do (println "You can [A]ttend the honors lecture, [W]ork on the honors project, or [T]alk to Prof. Beckman. Or [L]eave")
              (let [choice (read-line)
                    ho-und (get-in state [:student :honors :understanding])
                    ho-proj (get-in state [:student :honors :project])]
                (cond (= choice "A")
                      (do (println "You chose to attend the honors lecture.")
                          (assoc-in state [:student :honors :understanding] (+ ho-und 25)))
                      (= choice "W")
                      (do (println "You chose to work on the honors project.")
                          (assoc-in state [:student :honors :project] (+ ho-proj ho-und)))
                      (= choice "T")
                      (do (println "You chose to talk to Professor Beckman")
                          (assoc-in state [:student :honors :understanding] (+ ho-und 50)))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :eceb)
          (do (println "You can [A]ttend the lecture, [T]alk to Prof. Evans to increase your understanding. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "A")
                      (do (println "You chose to attend the honors lecture.")
                          (assoc-in state [:student :understanding] (+ und 25)))
                      (= choice "T")
                      (do (println "You chose to talk to Professor Evans")
                          (assoc-in state [:student :understanding] (+ und 50)))
                      (= choice "L")
                      (do (println "You left examining") state)))))))

(defn respond [state line]
  (cond (= line "M") (do (println "You choose to move. Which direction? [N]orth/[S]outh/[W]est/[E]ast?")
                         (let [choice (read-line)]
                           (cond (= choice "N")
                                 (do (println "you are tring to go north")
                                     (go state :north))
                                 (= choice "S")
                                 (do (println "You are trying to go south.")
                                     (go state :south))
                                 (= choice "W")
                                 (do (println "You are trying to go west.")
                                     (go state :west))
                                 (= choice "E")
                                 (do (println "You are trying to go east.")
                                     (go state :east)))))
        (= line "E") (do (println "You choose to examine") (building state))
        (= line "Q") (do (println "Thanks for playing!") (assoc-in state [:student :win] -1))))

(defn repl [state]
  (loop [state state]
    (cond (= (get-in state [:student :progress]) 100) (println "YOU WON!")
          (= (get-in state [:student :win]) 0) (if (< (get-in state [:student :progress]) 100)
                                                 (do (status state)
                                                     (println "You have not completed the task.")
                                                     (println "What do you want to do? ([M]ove/[E]xamine/[Q]uit) ")
                                                     (let [command (read-line)]
                                                       (recur (respond state command)))) (println "Game over"))
          (= (get-in state [:student :win]) 1) (println "YOU WON!")
          (= (get-in state [:student :win]) -1) (println "YOU LOST!"))))

(defn -main
  "Initialize the CS225."
  [& args]
  (repl init-state))


;; (defn foo
;;   "I don't do a whole lot."
;;   [x]
;;   (println x "Hello, World!"))
