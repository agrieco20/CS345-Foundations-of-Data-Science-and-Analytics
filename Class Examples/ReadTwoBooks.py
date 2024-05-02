from datascience import *
from urllib.request import urlopen
import numpy as np
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plots

def main():
    # Read two books, fast!

    huck_finn_url = 'https://www.inferentialthinking.com/data/huck_finn.txt'
    response = urlopen(huck_finn_url)
    huck_finn_text = str(response.read())
    huck_finn_chapters = huck_finn_text.split('CHAPTER ')[44:]

    little_women_url = 'https://www.inferentialthinking.com/data/little_women.txt'
    response = urlopen(little_women_url)
    little_women_text = str(response.read())
    little_women_chapters = little_women_text.split('CHAPTER ')[1:]

    Table().with_column('Chapters', huck_finn_chapters)

    # Get the cumulative counts the names Jim, Tom, and Huck appear in each chapter.

    counts = Table().with_columns([
        'Jim', np.cumsum(np.char.count(huck_finn_chapters, 'Jim')),
        'Tom', np.cumsum(np.char.count(huck_finn_chapters, 'Tom')),
        'Huck', np.cumsum(np.char.count(huck_finn_chapters, 'Huck'))
    ])

    # Plot the cumulative counts:
    # how many times in Chapter 1, how many times in Chapters 1 and 2, and so on.

    cum_counts = counts.with_column('Chapter', np.arange(1, 44, 1))
    cum_counts.plot(column_for_xticks=3)
    plots.title('Cumulative Number of Times Each Name Appears', y=1.08)

    # The chapters of Little Women, in a table

    Table().with_column('Chapters', little_women_chapters)

    # Get the cumulative counts of the names in the chapters of Little Women

    counts = Table().with_columns([
        'Amy', np.cumsum(np.char.count(little_women_chapters, 'Amy')),
        'Beth', np.cumsum(np.char.count(little_women_chapters, 'Beth')),
        'Jo', np.cumsum(np.char.count(little_women_chapters, 'Jo')),
        'Meg', np.cumsum(np.char.count(little_women_chapters, 'Meg')),
        'Laurie', np.cumsum(np.char.count(little_women_chapters, 'Laurie')),

    ])

    # Plot the cumulative counts.

    cum_counts = counts.with_column('Chapter', np.arange(1, 48, 1))
    cum_counts.plot(column_for_xticks=5)
    plots.title('Cumulative Number of Times Each Name Appears', y=1.08)

if __name__ == "__main__":
    main()