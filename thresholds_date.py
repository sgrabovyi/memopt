# Дата порогів: день, коли пороги востаннє змінились, а не час останнього запуску. Повторний розрахунок з тими самими
# порогами дату не змінює. Визначається за архівом output/archive/memopt_thresholds_РРРР-ММ-ДД_ГГХХ.csv: береться
# найраніша копія поспіль, у якої чотири пороги всіх територій збігаються з поточними.
import glob
import os
import re
from datetime import date

import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
COLS = ["base", "medium", "high", "veryhigh"]


def _values(path):
    t = pd.read_csv(path)
    return t.set_index("id")[COLS].round(2).sort_index()


def thresholds_date(fmt="%d.%m.%Y"):
    cur_f = os.path.join(HERE, "output", "memopt_thresholds.csv")
    cur = _values(cur_f)
    since = date.fromtimestamp(os.path.getmtime(cur_f))
    arch = sorted(glob.glob(os.path.join(HERE, "output", "archive", "memopt_thresholds_*.csv")), reverse=True)
    for f in arch:
        m = re.search(r"(\d{4})-(\d{2})-(\d{2})", os.path.basename(f))
        if not m:
            continue
        try:
            same = _values(f).equals(cur)
        except Exception:
            same = False
        if not same:
            break
        since = date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    return since.strftime(fmt)


if __name__ == "__main__":
    print(thresholds_date())
