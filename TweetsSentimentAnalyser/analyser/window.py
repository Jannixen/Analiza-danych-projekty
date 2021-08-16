import tkinter as tk

from tweet_analyser import analyse_tweet


def make_window():
    app = SentimentAnalyserAppWindow(500, 500, "Sentiment Analyser")
    app.mainloop()


class AppWindow(tk.Tk):

    def __init__(self, width, height, title):
        super().__init__()
        self.geometry(str(width) + "x" + str(height))
        self._title_label = self._make_title_label(title)
        self._entry = self._make_entry()
        self._action_button = self._make_action_button()
        self._result_label = self._make_result_label()

    def _make_title_label(self, title):
        title_label = tk.Label(self, text=title, padx=5, pady=5, font=("Arial", 30))
        title_label.place(relwidth=0.8, relheight=0.1, relx=0.1, rely=0.05)
        return title_label

    def _make_entry(self):
        entry = tk.Entry(self, borderwidth=5)
        entry.place(relwidth=0.8, relheight=0.5, relx=0.1, rely=0.2)
        entry.focus_set()
        return entry

    def _make_action_button(self):
        action_button = tk.Button(self, text="Analyse", bg="lightgrey", command=self._act_on_button)
        action_button.place(relwidth=0.8, relheight=0.1, relx=0.1, rely=0.70)
        return action_button

    def _make_result_label(self):
        result_label = tk.Label(self, text='Result will be shown here', borderwidth=3, relief="ridge", padx=5, pady=10,
                                font=("Arial", 15))
        result_label.place(relwidth=0.8, relheight=0.1, relx=0.1, rely=0.85)
        return result_label

    def _act_on_button(self):
        pass


class SentimentAnalyserAppWindow(AppWindow):

    def __init__(self, width, height, title):
        super().__init__(width, height, title)

    def _show_positive_result(self):
        self._result_label['text'] = "Positive sentiment"
        self._result_label['bg'] = 'green'

    def _show_negative_result(self):
        self._result_label['text'] = "Negative sentiment"
        self._result_label['bg'] = 'red'

    def _show_null_result(self):
        self._result_label['text'] = "No sentiment"
        self._result_label['bg'] = 'white'

    def _act_on_button(self):
        if not self._entry.get():
            self._show_null_result()
        else:
            prediction = analyse_tweet(self._entry.get())
            if prediction == [0]:
                self._show_negative_result()
            else:
                self._show_positive_result()
