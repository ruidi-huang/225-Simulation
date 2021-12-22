(ns adventure.core
  (:gen-class))

(def init-map {:bar {:desc "This is a bar where you can relax and have fun with your friends. You can reduce some pressures in this place.",
                     :title "in the bar",
                     :dir {:east :home},
                     :contents #{:beer}},
               :home {:desc "This is your home. You can sleep here to restore your energy."
                      :title "in your home",
                      :dir {:west :bar, :east :restaurant},
                      :content #{:bed}},
               :restaurant {:desc "This is a restaurant on green street. You can eat here to increase your food point.",
                            :title "in the restaurant",
                            :dir {:south :altgeld, :west :home, :east :grainger},
                            :content #{:food :take-out}},
               :altgeld {:desc "This is Altgeld Hall. The legends says that there is a mysterious Dairy Queen down in the basement of Altgeld. If you find it, you will automically win, otherwise you'll lose.",
                         :title "in Altgeld Hall",
                         :dir {:north :restaurant, :east :starbucks},
                         :content #{:possibly-existed-Dairy-Queen}},
               :starbucks {:desc "This is the starbucks in the Illini Union. You can get some coffee to restore a small portion of your energy.",
                           :title "in Starbucks",
                           :dir {:north :grainger, :west :altgeld},
                           :content #{:coffee}},
               :grainger {:desc "This is Grainger Library. You can fetch, work on, and push your MP and take your exam here",
                          :title "in Grainger Library",
                          :dir {:north :digital, :west :restaurant, :south :starbucks, :east :isr},
                          :content #{:linuxMachine, :cbtf}},
               :isr {:desc "This is ISR dining hall. You can eat here and restore your food point and restore your wellness.",
                     :title "in ISR",
                     :dir {:west :grainger},
                     :content #{:dining}},
               :digital {:desc "This is Digital Lab. You can fetch, work on, and push your MP here.",
                         :title "in Digital Lab",
                         :dir {:north :siebel, :south :grainger},
                         :content #{:linuxMachine}},
               :siebel {:desc "This is Siebel Center for Computer Science. You can attend the honors lecture taught by Prof. Beckman, talk to him, and work on your honors project here.",
                        :title "in Siebel Center",
                        :dir {:north :eceb, :south :digital},
                        :content #{:beckman}},
               :eceb {:desc "This is ECE Building. You can attend the lecture taught by Prof. Evans and talk to him.",
                      :title "in ECEB",
                      :dir {:south :siebel},
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
      (do (println "You can't go that way. Check the map too see where you can go!") state)
      (do (println "You are going to " dest) (assoc-in state [:student :location] dest)))))

(defn status [state]
  (let [loc (get-in state [:student :location])
        the-map (:map state)
        visited (get-in state [:student :seen])]
    (println (str "You are " (-> loc the-map :title) ". "))
    (when-not (contains? (get-in state [:student :seen]) loc)
      (println (-> the-map loc :desc)))
    (assoc-in state [:student :seen] (conj visited loc))))

(defn health [state]
  (let [pre (get-in state [:student :pressure])
        wel (get-in state [:student :wellness])
        pro (get-in state [:student :progress])
        foo (get-in state [:student :food])
        enr (get-in state [:student :energy])
        und (get-in state [:student :understanding])
        num (get-in state [:student :inventory :mp])
        hor-und (get-in state [:student :honors :understanding])
        hor-proj (get-in state [:student :honors :project])]
    (println "Your condition: ")
    (println "        Welness:" wel)
    (println "        Progress (need to get to 100 to pass):" pro)
    (println "        Food Point:" foo)
    (println "        Energy:" enr)
    (println "        understanding:" und)
    (println "        Pressure:" pre)
    (println "        MP degree of completion:" num)
    (println "        honors understanding:" hor-und)
    (println "        Honors Project degree of completion:" hor-proj)))

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
                      (do (println "You chose to drink. Pressure - 50. Wellness - 40.")
                          (-> state
                              (assoc-in [:student :pressure] (- pre 50))
                              (assoc-in [:student :wellness] (- wel 40))))
                      (= choice "P")
                      (do (println "You chose to party. Pressure - 20. Energy - 10")
                          (-> state
                              (assoc-in [:student :pressure] (- pre 20))
                              (assoc-in [:student :energy] (- enr 10))))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :home)
          (do (println "You can [S]leep to increase energy to maximum and reduce food point to 0. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "S")
                      (do (println "You chose to sleep. Energy: 100. Food: 0.")
                          (-> state
                              (assoc-in [:student :energy] 100)
                              (assoc-in [:student :food] 0)))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :restaurant)
          (do (println "You can [E]at to increase your food point. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "E")
                      (do (println "You chose to eat. Food point + 20")
                          (assoc-in state [:student :food] (+ foo 20)))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :altgeld)
          (do (println "You can try to [F]ind the DQ. If you find it, you will win the game! If you can't find it, you will lose the game! Or [L]eave")
              (let [choice (read-line)
                    chance (rand-int 2)]
                (cond (= choice "F")
                      (do (println "You chose to look for DQ")
                          (cond (= chance 1) (do (println "CONGRATS! You found the Altgeld DQ!") (assoc-in state [:student :win] 1))
                                :else (do (println "Oops! You did not find the Altgeld DQ") (assoc-in state [:student :win] -1))))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :starbucks)
          (do (println "You can [D]rink coffee to increase your energy point. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "D")
                      (do (println "You chose to drink coffee. Energy + 10, Wellness - 5")
                          (-> state
                              (assoc-in [:student :energy] (+ enr 10))
                              (assoc-in [:student :wellness] (- wel 5))))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :grainger)
          (do (println "You can [F]etch your MP, [W]ork on the MP, [P]ush your MP, or [T]ake the exam. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "F")
                      (cond (= num 0) (do (println "You chose to fetch your MP")
                                          (println "You have successfully fetched your MP")
                                          (assoc-in state [:student :inventory :mp] 1))
                            :else (do (println "You have already fetched your MP!") state))
                      (= choice "W")
                      (cond (> num 0) (do (println "You chose to work on the MP!")
                                          (println "You before have completed " (-> state :student :inventory :mp) "% of the MP")
                                          (assoc-in state [:student :inventory :mp] (- (+ (get-in state [:student :inventory :mp]) (/ und 10) (/ enr 10) (/ foo 10) (/ wel 5)) (/ pre 5)))
                                          ;; (if (> (get-in state [:student :inventory :mp]) 100) (assoc-in state [:student :inventory :mp] 100) ())
                                          )
                            :else (do (println "You have to fetch your MP first!") state))
                      (= choice "P") (cond (> num 0) (do (println "You chose to push the MP!")
                                                         (println "you got a " (-> state :student :inventory :mp) "% for the MP!")
                                                         (-> state
                                                             (assoc-in  [:student :mp-score] (-> state :student :inventory :mp))
                                                             (assoc-in [:student :progress] (+ 50 pro))))
                                           :else (do (println "You have to fetch your MP first!") state))
                      (= choice "T") (do (println "You chose to take the exam!")
                                         (println "You got a " und "% on the exam!")
                                         (-> state
                                             (assoc-in  [:student :inventory :exam] und)
                                             (assoc-in [:student :progress] (+ 50 pro))))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :isr)
          (do (println "You can [E]at at the ISR dining hall to increase your food point and wellness. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "E")
                      (do (println "You chose to eat. Food point + 25, wellness + 10")
                          (-> state
                              (assoc-in [:student :food] (+ foo 30))
                              (assoc-in [:student :wellness] (+ wel 10))))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :digital)
          (do (println "You can [F]etch your MP, [W]ork on the MP, or [P]ush your MP. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "F")
                      (cond (= num 0) (do (println "You chose to fetch your MP")
                                          (println "You have successfully fetched your MP")
                                          (assoc-in state [:student :inventory :mp] 1))
                            :else (do (println "You have already fetched your MP!") state))
                      (= choice "W")
                      (cond (> num 0) (do (println "You chose to work on the MP!")
                                          (println "You before have completed " (-> state :student :inventory :mp) "% of the MP")
                                          (assoc-in state [:student :inventory :mp] (- (+ (get-in state [:student :inventory :mp]) (/ und 10) (/ enr 10) (/ foo 10) (/ wel 5)) (/ pre 5)))
                                          ;; (if (> (get-in state [:student :inventory :mp]) 100) (assoc-in state [:student :inventory :mp] 100) ())
                                          )
                            :else (do (println "You have to fetch your MP first!") state))
                      (= choice "P") (cond (> num 0) (do (println "You chose to push the MP!")
                                                         (println "you got a " (-> state :student :inventory :mp) "% for the MP!")
                                                         (-> state
                                                             (assoc-in  [:student :mp-score] (-> state :student :inventory :mp))
                                                             (assoc-in [:student :progress] (+ 50 pro))))
                                           :else (do (println "You have to fetch your MP first!") state))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :siebel)
          (do (println "You can [A]ttend the honors lecture, [W]ork on the honors project, or [T]alk to Prof. Beckman. Or [L]eave")
              (let [choice (read-line)
                    ho-und (get-in state [:student :honors :understanding])
                    ho-proj (get-in state [:student :honors :project])]
                (cond (= choice "A")
                      (do (println "You chose to attend the honors lecture.")
                          (println "Clojure understanding + 25")
                          (assoc-in state [:student :honors :understanding] (+ ho-und 25)))
                      (= choice "W")
                      (do (println "You chose to work on the honors project.")
                          (assoc-in state [:student :honors :project] (+ ho-proj ho-und)))
                      (= choice "T")
                      (do (println "You chose to talk to Professor Beckman")
                          (println "Clojure understanding + 50")
                          (assoc-in state [:student :honors :understanding] (+ ho-und 50)))
                      (= choice "L")
                      (do (println "You left examining") state))))
          (= loc :eceb)
          (do (println "You can [A]ttend the lecture, [T]alk to Prof. Evans to increase your understanding. Or [L]eave")
              (let [choice (read-line)]
                (cond (= choice "A")
                      (do (println "You chose to attend the lecture.")
                          (assoc-in state [:student :understanding] (+ und 25)))
                      (= choice "T")
                      (do (println "You chose to talk to Professor Evans")
                          (assoc-in state [:student :understanding] (+ und 50)))
                      (= choice "L")
                      (do (println "You left examining") state)))))))

(defn showmap []
  (println "  Map:                        ")
  (println "                  North            ")
  (println "                    ^          ")
  (println "                    |         ")
  (println "         West   <--   -->    East    ")
  (println "                    |         ")
  (println "                    v    ")
  (println "                   South    ")
  (println "                       ")
  (println "                                             ECE Building      ")
  (println "                                             Siebel Center      ")
  (println "                                              Digital Lab      ")
  (println "            Bar     Home      Restaurant       Grainger      ISR   ")
  (println "                             Altgeld Hall      Starbucks      ")
  (println "                       ")
  (println "                       ")
  (println "                       "))

(defn respond [state]
  (println "What do you want to do? ([M]ove/[E]xamine building/[C]heck your condition/[V]iew the map/[Q]uit) ")
  (let [line (read-line)] (cond (= line "M") (do (println "You choose to move. Which direction? [N]orth/[S]outh/[W]est/[E]ast?")
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
                                (= line "V") (do (showmap) state)
                                (= line "C") (do (println "You choose to check your condition") (health state) state)
                                (= line "Q") (do (println "Thanks for playing!") (assoc-in state [:student :win] -1)))))

(defn repl [state]
  (loop [state state]
    (cond (= (get-in state [:student :progress]) 100) (println "YOU WON with the exam score" (get-in state [:student :inventory :exam]) ", MP score " (get-in state [:student :mp-score]) ", and honors Project score" (get-in state [:student :honors :project]))
          (= (get-in state [:student :win]) 0) (if (< (get-in state [:student :progress]) 100)
                                                  ;;  (status state)
                                                 (recur (respond (status state))) (println "Game over"))
          (= (get-in state [:student :win]) 1) (println "YOU WON!")
          (= (get-in state [:student :win]) -1) (println "YOU LOST!"))))

(defn -main
  "Initialize the CS225."
  [& args]
  (println "Welcome to the game of CS225. You are a student who is taking CS225. You need to Push your MP and take your exam in order to pass the class (win the game). 
            You have several indexes you need to keep up with: Welness, Food Point, Energy, Understanding, Pressure, Honors-understading.
            There are a lot of other things to do such as go to a bar, get some food, attend lecture.
            All these events can affect your indexes, which can infect your productivity on MP and exam score. 
            Additionally, there is one mysterious spot at Altgeld Hall that may surprise you!
            Good luck! Hope you pass this class!")
  (println "")
  (repl init-state))


;; (defn foo
;;   "I don't do a whole lot."
;;   [x]
;;   (println x "Hello, World!"))
