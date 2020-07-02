(ns mazeprim.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [clojure.set]))

(def size 38)
(def unit 20)

(defn bounds-check [[x y]]
  (if (and (>= x 0) (< x size)
           (>= y 0) (< y size)) [x y]))

(defn init-cell []
  [(rand-int size) (rand-int size)])

(defn adj-cell [[x y] dir]
  (bounds-check (cond (= dir 'north) [x (dec y)]
                      (= dir 'south) [x (inc y)]
                      (= dir 'east) [(inc x) y]
                      (= dir 'west) [(dec x) y])))

(defn adj-cells [cell]
  (filter #(not (nil? %)) (map #(adj-cell cell %) '(north south east west))))

(defn point [cell]
  (vec (map #(* % unit) cell)))

(defn eastwall-pp [cell]
  (let [n (adj-cell cell 'east)]
    (if n (let [[x y] n
                y' (inc y)]
            (list (point n) (point [x y']))))))

(defn westwall-pp [cell]
  (let [n (adj-cell cell 'west)]
    (if n (let [[x y] cell
                y' (inc y)]
            (list (point cell) (point [x y']))))))

(defn northwall-pp [cell]
  (let [n (adj-cell cell 'north)]
    (if n (let [[x y] cell
                x' (inc x)]
            (list (point cell) (point [x' y]))))))

(defn southwall-pp [cell]
  (let [n (adj-cell cell 'south)]
    (if n (let [[x y] n
                x' (inc x)]
            (list (point n) (point [x' y]))))))

(defn init-walls []
  (let [cells (for [x (range size) y (range size)] [x y])]
    (set (filter #(not (nil? %))
                 (concat (map eastwall-pp cells) (map southwall-pp cells))))))

(defn next-cell
  "Returns a cell from the set of frontier cells."
  [state]
  (let [cells (:frontier state)]
    (if (empty? cells) nil
        (rand-nth (seq cells)))))

(defn maze-cell
  "Return a cell already in the maze which is adjacent to the given frontier cell."
  [frontier-cell maze]
  (rand-nth (seq (intersection (set (adj-cells frontier-cell)) maze))))

(defn bridge
  "Return the bridge (wall to remove) between the given frontier cell and in-maze cell."
  [frontier-cell in-maze-cell]
  (let [[cx cy] frontier-cell
        [bx by] in-maze-cell]
    (cond (> bx cx) (eastwall-pp frontier-cell)
          (< bx cx) (westwall-pp frontier-cell)
          (> by cy) (southwall-pp frontier-cell)
          (< by cy) (northwall-pp frontier-cell))))

(defn cross-bridge [frontier-cell state]
  (let [{:keys [walls maze frontier]} state
        in-maze (maze-cell frontier-cell maze)]
    (disj walls (bridge frontier-cell in-maze))))

(defn setup []
  (q/frame-rate 15)
  (let [cell (init-cell)]
    {:walls (init-walls)
     :maze #{cell}
     :frontier (set (adj-cells cell))}))

(defn update-state [state]
  (let [next (next-cell state)
        {:keys [walls  maze frontier]} state]
    (if (nil? next) state
        {:walls (cross-bridge next state)
         :maze (conj maze next)
         :frontier (difference (union (disj frontier next)
                                      (set (adj-cells next))) maze)})))

(defn draw-frontier [l]
  (if (not (empty? l))
    (doall (map #(let [[x y] %] (q/rect (* x unit) (* y unit) unit unit)) (seq l)))))

(defn draw-state [state]
  (q/background 240)
  (q/translate 20 20)
  (q/fill 255 102 102)
  (draw-frontier (seq (:frontier state)))
  (q/stroke 0)
  (doall (map #(apply q/line %) (:walls state)))
  (q/no-fill)
  (q/rect 0 0 (* size unit) (* size unit)))

(q/defsketch mazeprim
  :title "Maze Generator - Prim's Algorithm"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
