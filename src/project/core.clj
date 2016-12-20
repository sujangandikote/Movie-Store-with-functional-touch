(ns project.core
  (:gen-class)
  (:require [seesaw.core :as seesaw]
            [seesaw.swingx :as swingx]
            [clj-time.core :as time]))

(defn display
  [content width height]
  (let [window (seesaw/frame :title "Welcome to the world of Movies"
                             :content content
                             :width width
                             :height height)]
    (seesaw/show! window)))

(def reading (read-string (slurp "available-movie.txt")))

(def reading-renters-info (read-string (slurp "renters-info.txt")))


(def movie-available (seesaw/table
                      :model [
                      :columns [{:key :name, :text "Movie-Name"} :price :quantity :id]
                      :rows reading]))

(def renters-info (seesaw/table
                   :model [
                   :columns [{:key :name, :text "Movie-Name"} :renter-name {:key :due-date, :text "Due-Date(YYYY-MM-DD)"}]
                   :rows reading-renters-info]))

(defn add_movie []
  (seesaw/show! (seesaw/dialog
                 :title "Add movie"
                 :height 300
                 :width 600
                 :modal? true
                 :content
                 (seesaw/form-panel
                  :items[[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
                         [(seesaw/label :text "Movie" :halign :right)]
                         [(seesaw/text :id :movie :columns 20) :grid :next :weightx 1.0]
                         [(seesaw/label :text "Price($)" :halign :right) :gridheight 1 :grid :wrap]
                         [(seesaw/text :id :price :columns 5) :grid :next :weightx 1.0]
                         [(seesaw/label :text "Quantity" :halign :right) :gridheight 2 :grid :wrap]
                         [(seesaw/text :id :copies :columns 5) :grid :next :weightx 1.0]
                         [[5 :by 5] :grid :wrap]])
                 :option-type :ok-cancel
                 :success-fn (fn [p](try (let [new-movie (seesaw/text (seesaw/select (seesaw/to-root p) [:#movie]))
                                           price (read-string (seesaw/text (seesaw/select (seesaw/to-root p) [:#price])))
                                           copies (read-string (seesaw/text (seesaw/select (seesaw/to-root p) [:#copies])))
                                           row (seesaw.table/row-count movie-available)
                                           id (inc row)
                                           reading (read-string (slurp "available-movie.txt"))
                                           k (assoc-in reading [(count reading)] {:name new-movie :price price :quantity copies :id id})
                                           p  (spit "available-movie.txt" k)]
                                       (seesaw.table/insert-at! movie-available row [new-movie price copies id])
                                       (seesaw/alert (print-str "New Movie added is" new-movie)))
                               (catch Exception e (seesaw/alert (print-str "Fields cannot be left blank!"))))))))





(defn rent_movie [event]
 (try (let [selected (seesaw/selection movie-available {:multi false})
        available-quantity (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:quantity])))
        name-of-movie (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:name])))
        price-of-movie (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:price])))
        movie-id (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:id])))
        due-date (str (time/plus (time/today) (time/weeks 2)))]
    (if (< 0 available-quantity)
      (seesaw/show! (seesaw/dialog
                     :title "Renting movie"
                     :height 400
                     :width 400
                     :modal? true
                     :content
                     (seesaw/form-panel
                      :items[[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
                             [(seesaw/label :text "Renter name" :halign :right)]
                             [(seesaw/text :id :renter-name :columns 20) :grid :next :weightx 1.0]
                             [[5 :by 5] :grid :wrap]])
                     :option-type :ok-cancel
                     :success-fn (fn [q] (let [name-of-renter (seesaw/text (seesaw/select (seesaw/to-root q) [:#renter-name]))
                                               row (seesaw.table/row-count renters-info)
                                               reading (read-string (slurp "available-movie.txt"))
                                               reading-renter (read-string (slurp "renters-info.txt"))
                                               k (assoc-in reading-renter [row] {:name name-of-movie :renter-name name-of-renter :due-date due-date})
                                               p (spit "renters-info.txt" k)
                                               k1 (assoc-in reading [selected] {:name name-of-movie :price price-of-movie :quantity (dec available-quantity) :id movie-id})
                                               p1 (spit "available-movie.txt" k1)]
                                           (seesaw.table/insert-at! renters-info row [name-of-movie name-of-renter due-date])
                                           (seesaw.table/update-at! movie-available selected [name-of-movie price-of-movie (dec available-quantity) movie-id])
                                           (seesaw/alert (print-str "Rented movie is" name-of-movie "and due date is" due-date))))))
      (seesaw/alert (print-str "movie currently not available"))))
   (catch Exception e (seesaw/alert (print-str "Please select a movie to rent!")))))




(defn update_price []
  (seesaw/show! (seesaw/dialog
                 :title "Update Price"
                 :height 300
                 :width 600
                 :modal? true
                 :content
                 (seesaw/form-panel
                  :items[[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
                         [(seesaw/label :text "Enter new Price" :halign :right)]
                         [(seesaw/text :id :updated-price :columns 20) :grid :next :weightx 1.0]
                         [[5 :by 5] :grid :wrap]])
                 :option-type :ok-cancel
                 :success-fn (fn [q] (try (let [updated-price (read-string (seesaw/text (seesaw/select (seesaw/to-root q) [:#updated-price])))
                                           selected (seesaw/selection movie-available {:multi false})
                                           pre-value (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:price])))
                                           movie (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:name])))
                                           quantity (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:quantity])))
                                           id (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:id])))
                                           reading (read-string (slurp "available-movie.txt"))
                                           k (assoc-in reading [selected] {:name movie :price updated-price :quantity quantity :id id})
                                           p  (spit "available-movie.txt" k)]
                                       (seesaw.table/update-at! movie-available selected [movie updated-price quantity id])
                                       (seesaw/alert (print-str "updated price is" updated-price)))
                                       (catch Exception e (seesaw/alert (print-str "Please Enter some value or u have not selected any movie!"))))))))



(defn update_quantity []
  (seesaw/show! (seesaw/dialog
                 :title "Update Quantity"
                 :height 300
                 :width 600
                 :modal? true
                 :content
                 (seesaw/form-panel
                  :items[[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
                         [(seesaw/label :text "Enter new Quantity" :halign :right)]
                         [(seesaw/text :id :updated-quantity :columns 20) :grid :next :weightx 1.0]
                         [[5 :by 5] :grid :wrap]])
                 :option-type :ok-cancel
                 :success-fn (fn [q] (try (let [updated-quantity (read-string (seesaw/text (seesaw/select (seesaw/to-root q) [:#updated-quantity])))
                                           selected (seesaw/selection movie-available {:multi false})
                                           pre-value (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:quantity])))
                                           movie (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:name])))
                                           price (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:price])))
                                           id (first (vals (select-keys (seesaw.table/value-at movie-available selected) [:id])))
                                           reading (read-string (slurp "available-movie.txt"))
                                           k (assoc-in reading [selected] {:name movie :price price :quantity updated-quantity :id id})
                                           p  (spit "available-movie.txt" k)]
                                       (seesaw.table/update-at! movie-available selected [movie price updated-quantity id])
                                       (seesaw/alert (print-str "no of copies updated to" updated-quantity)))
                                       (catch Exception e (seesaw/alert (print-str "Please Enter some value or u have not selected any movie!"))))))))



(defn search_movie []
  (seesaw/show! (seesaw/dialog
                 :title "Search Movie by either name or id"
                 :height 200
                 :width 500
                 :modal? true
                 :content
                 (seesaw/form-panel
                  :items[[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
                         [(seesaw/label :text "Enter Movie by Either name or ID(Case Sensitive)" :halign :right)]
                         [(seesaw/text :id :name-or-id :columns 20) :grid :next :weightx 1.0]
                         [[5 :by 5] :grid :wrap]])
                 :option-type :ok-cancel
                 :success-fn (fn [e] (try (let [movie-or-id (seesaw/text (seesaw/select (seesaw/to-root e) [:#name-or-id]))
                                           rowcount (seesaw.table/row-count movie-available)
                                           a  (or (first (filter #(= (:name %) movie-or-id) (seesaw.table/value-at movie-available (vec (range rowcount)))))
                                                  (first (filter #(= (:id %) (read-string movie-or-id)) (seesaw.table/value-at movie-available (vec (range rowcount))))))
                                           price (first (vals (select-keys a [:price])))
                                           quantity (first (vals (select-keys a [:quantity])))
                                           name-of-movie (first (vals (select-keys a [:name])))]
                                       (if (empty? a)
                                         (seesaw/alert (print-str "Sorry! Movie you were trying to search is not available"))
                                         (seesaw/alert (print-str "Movie you were searching is " name-of-movie "and currrently" quantity "copies available and"
                                                                  "price of the movie is" price))))
                                       (catch Exception e (seesaw/alert (print-str "Please Enter movie name or id !"))))))))





(defn return_movie []
  (try (let [selected (seesaw/selection renters-info {:multi false})
        name-of-movie (first (vals (select-keys (seesaw.table/value-at renters-info selected) [:name])))
        rowcount (seesaw.table/row-count movie-available)
        a  (first (filter #(= (:name %) name-of-movie) (seesaw.table/value-at movie-available (vec (range rowcount)))))
        price (first (vals (select-keys a [:price])))
        quantity (first (vals (select-keys a [:quantity])))
        id (first (vals (select-keys a [:id])))
        reading (read-string (slurp "available-movie.txt"))
        reading-renter (read-string (slurp "renters-info.txt"))
        index (.indexOf reading a)
        k (vec (filter #(not= (nth reading-renter selected) %) reading-renter))
        p  (spit "renters-info.txt" k)
        k1 (assoc-in reading [index] {:name name-of-movie :price price :quantity (inc quantity) :id id})
        p1 (spit "available-movie.txt" k1)]
        (seesaw.table/update-at! movie-available (dec id) [name-of-movie price (inc quantity) id])
    (seesaw.table/remove-at! renters-info selected)
    (seesaw/alert (print-str "Successfully returned the movie" name-of-movie)))
    (catch Exception e (seesaw/alert (print-str "Please select a movie to return to store !")))))



(defn remove_movie []
  (try (let [selected-row (seesaw/selection movie-available {:multi false})
        movie-deleted (first (vals (select-keys (seesaw.table/value-at movie-available selected-row) [:name])))
        available-quantity (first (vals (select-keys (seesaw.table/value-at movie-available selected-row) [:quantity])))
        price-of-movie (first (vals (select-keys (seesaw.table/value-at movie-available selected-row) [:price])))
        id (first (vals (select-keys (seesaw.table/value-at movie-available selected-row) [:id])))]
    (if (< 0 available-quantity)
      (let [decrease_quantity  (seesaw.table/update-at! movie-available selected-row [movie-deleted price-of-movie (dec available-quantity) id])
            reading (read-string (slurp "available-movie.txt"))
            k (assoc-in reading [selected-row] {:name movie-deleted :price price-of-movie :quantity (dec available-quantity) :id id})
            p  (spit "available-movie.txt" k)]
        (seesaw/alert (print-str "1 copy of" movie-deleted "is removed")))
      (seesaw/alert (print-str "Movie cannot be Deleted"))))
  (catch Exception e (seesaw/alert (print-str "You did not select any movie to delete !")))))



(def remove-movie (seesaw/button :text "Remove Movie"
                                :listen [:action (fn [event]
                                                 (remove_movie))]))



(def rent-movie (seesaw/button :text "Rent Movie"
                               :listen [:action (fn [event]
                                                  (rent_movie event))]))


(def add-movie (seesaw/button :text "Add Movie"
                                :listen [:action (fn [event]
                                                 (add_movie))]))


(def update-price (seesaw/button :text "Update Price"
                                 :listen [:action (fn [event]
                                                    (update_price))]))

(def update-quantity (seesaw/button :text "Update Quantity"
                                 :listen [:action (fn [event]
                                                    (update_quantity))]))

(def return-movie (seesaw/button :text "Return Movie"
                                 :listen [:action (fn [event]
                                                    (return_movie))]))

(def search-movie (seesaw/button :text "Search Movie"
                                 :listen [:action (fn [event]
                                                    (search_movie))]))


(def panel
  (seesaw/flow-panel :items [(seesaw/scrollable movie-available) add-movie remove-movie rent-movie update-price update-quantity search-movie]))


(def renters-panel
  (seesaw/flow-panel :items [(seesaw/scrollable renters-info) return-movie]))


(def tabs (seesaw/tabbed-panel
           :placement :top
           :tabs [{:title "Movies available"
                   :content panel}
                  {:title "Renters Information"
                   :content renters-panel}]))



(defn -main
  [& args]
  (display tabs 1800 1000))

