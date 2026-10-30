# Рисунки для статті. Усі дані беруться з output/ memopt (5 сезонів), крім рис. 4, де числа восьмисезонного
# розрахунку взято з попереднього запуску (config_8seasons_backup.json), див. коментар.
import os
import numpy as np, pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
M = os.path.dirname(os.path.abspath(__file__))
os.chdir(M)
FIG = "docs/paper/fig"
os.makedirs(FIG, exist_ok=True)
plt.rcParams.update({"font.family": "DejaVu Sans", "font.size": 9, "axes.spines.top": False, "axes.spines.right": False})
WEEKS = list(range(40, 53)) + list(range(1, 21))
LAB = [str(w) for w in WEEKS]
F = pd.read_csv("output/reference_series_filled.csv")
T = pd.read_csv("output/memopt_thresholds.csv").set_index("id")
X = pd.read_excel("output/" + os.environ.get("MO_RESULTS", "memopt_results.xlsx"), sheet_name=None)
SM = X["Малі території"].set_index("Код")
TN = ["епідемічний", "середній", "високий", "дуже високий"]
TC = ["#1f77b4", "#e6b800", "#e07b00", "#c00000"]


def series(id_, S):
    s = F[(F.id == id_) & (F.season == S)].set_index("week").rate_filled.reindex(WEEKS)
    return s.values


# ---------- Рис. 1: область, п'ять сезонів і пороги
seasons = sorted(F[F.id == "UA59"].season.unique())
fig, ax = plt.subplots(figsize=(7.2, 3.6))
cols = plt.cm.viridis(np.linspace(0, 0.9, len(seasons)))
for s, c in zip(seasons, cols):
    ax.plot(range(len(WEEKS)), series("UA59", s), color=c, lw=1.4, label=s)
for n, c, k in zip(TN, TC, ("base", "medium", "high", "veryhigh")):
    ax.axhline(T.loc["UA59", k], color=c, ls="--", lw=1, label=f"{n} поріг {T.loc['UA59', k]:.0f}")
ax.set_xticks(range(0, len(WEEKS), 2)); ax.set_xticklabels(LAB[::2])
ax.set_xlabel("Тиждень сезону"); ax.set_ylabel("Випадків ГРВІ на 100 тис. за тиждень")
ax.legend(fontsize=7, ncol=2, frameon=False)
fig.tight_layout(); fig.savefig(f"{FIG}/fig1_oblast.png", dpi=220); plt.close(fig)

# ---------- Рис. 2: вага власного порогу
W = X["Ваги малих територій"]
cstar = float(((1 / W["Вага власного порогу"] - 1) * W["Випадків на тиждень"]).median())
fig, ax = plt.subplots(figsize=(5.2, 3.3))
x = np.linspace(0.3, 400, 500)
ax.plot(x, 1 / (1 + cstar / x), color="#1f77b4", lw=1.5, label=f"w = n / (n + c*), c* = {cstar:.1f}")
ax.scatter(W["Випадків на тиждень"].clip(lower=0.3), W["Вага власного порогу"], s=14, color="#c00000", zorder=3, label="51 громада")
ax.set_xscale("log"); ax.set_xticks([1, 3, 10, 30, 100, 300]); ax.set_xticklabels(["1", "3", "10", "30", "100", "300"])
ax.set_xlabel("Медіана випадків на тиждень, n (логарифмічна шкала)"); ax.set_ylabel("Вага власного порогу, w")
ax.set_ylim(0, 1.02); ax.legend(frameon=False, fontsize=8)
fig.tight_layout(); fig.savefig(f"{FIG}/fig2_weight.png", dpi=220); plt.close(fig)

# ---------- Рис. 3: правило підтвердження районом на прикладі Новослобідської громади, сезон 2025/2026
hid, S, ref = "UA5902011", "2025/2026", "UA5902_nc"
h = pd.read_excel("input/hromadas.xlsx", sheet_name=hid).set_index("Week")[S].reindex(WEEKS).values
r = series(ref, S)
t = [SM.loc[hid, c] for c in ("Рекомендований епідемічний поріг", "Рекомендований середній поріг", "Рекомендований високий поріг", "Рекомендований дуже високий поріг")]
rb = T.loc[ref, "base"]
fig, axes = plt.subplots(2, 1, figsize=(7.2, 5.2), sharex=True, gridspec_kw={"height_ratios": [1.3, 1]})
ax = axes[0]
ax.plot(range(len(WEEKS)), h, color="black", lw=1.4, marker="o", ms=3, label="громада")
for n, c, v in zip(TN, TC, t):
    ax.axhline(v, color=c, ls="--", lw=1, label=f"{n} поріг {v:.0f}")
own_mid = np.array([(v == v and v >= t[1]) for v in h])
gate = np.array([(v == v and v >= rb) for v in r])
for i in range(len(WEEKS)):
    if own_mid[i] and not gate[i]:
        ax.axvspan(i - 0.5, i + 0.5, color="#bbbbbb", alpha=0.5, lw=0)
ax.set_ylabel("Громада, на 100 тис.")
ax.set_title(f"Новослобідська сільська громада, сезон {S}. Сірим позначено тижні, у які діє правило підтвердження районом", fontsize=9)
ax.legend(fontsize=7, ncol=3, frameon=False, loc="upper left")
ax = axes[1]
ax.plot(range(len(WEEKS)), r, color="#1f77b4", lw=1.4, marker="o", ms=3, label="Конотопський район без центру")
ax.axhline(rb, color="#1f77b4", ls="--", lw=1, label=f"епідемічний поріг району {rb:.0f}")
ax.fill_between(range(len(WEEKS)), 0, np.where(gate, r, np.nan), color="#9DC3E6", alpha=0.5, step="mid", label="район не нижче свого епідемічного порогу")
ax.set_xticks(range(0, len(WEEKS), 2)); ax.set_xticklabels(LAB[::2])
ax.set_xlabel("Тиждень сезону"); ax.set_ylabel("Район, на 100 тис.")
ax.legend(fontsize=7, frameon=False, loc="upper left")
fig.tight_layout(); fig.savefig(f"{FIG}/fig3_district_confirm.png", dpi=220); plt.close(fig)

# ---------- Рис. 4: тижнів вище епідемічного порогу району в 2025/2026: пороги на 8 і на 5 сезонах
# Числа восьмисезонного варіанта взято з попереднього запуску memopt (config_8seasons_backup.json, 20.10.2026);
# п'ятисезонні перераховано тут із поточних файлів.
names8 = ["Конотопський район без центру", "Роменський район без центру", "Сумський район без центру", "Шосткинський район без центру", "Охтирський район без центру",
          "Конотопський район", "Роменський район", "Сумський район", "Шосткинський район", "Охтирський район"]
ids = ["UA5902_nc", "UA5906_nc", "UA5908_nc", "UA5910_nc", "UA5904_nc", "UA5902", "UA5906", "UA5908", "UA5910", "UA5904"]
w8 = {"UA5902_nc": 25, "UA5906_nc": 14, "UA5908_nc": 1, "UA5910_nc": 1, "UA5904_nc": 1, "UA5902": 13, "UA5906": 6, "UA5908": 3, "UA5910": 5, "UA5904": 4}
w5 = {}
for i in ids:
    s = series(i, "2025/2026"); w5[i] = int(np.nansum(s >= T.loc[i, "base"]))
print("тижнів над епідемічним порогом 2025/26 (5 сезонів):", w5)
fig, ax = plt.subplots(figsize=(7.2, 3.4))
y = np.arange(len(ids))
ax.barh(y - 0.2, [w8[i] for i in ids], 0.4, color="#bbbbbb", label="пороги на 8 сезонах (2012/13 до 2025/26)")
ax.barh(y + 0.2, [w5[i] for i in ids], 0.4, color="#1f77b4", label="пороги на 5 сезонах (2017/18 до 2025/26)")
ax.set_yticks(y); ax.set_yticklabels(names8, fontsize=8); ax.invert_yaxis()
ax.set_xlabel("Тижнів сезону 2025/2026 не нижче епідемічного порогу (з 33)")
ax.legend(frameon=False, fontsize=8)
fig.tight_layout(); fig.savefig(f"{FIG}/fig4_8vs5.png", dpi=220); plt.close(fig)

# ---------- Рис. 5: кут нахилу для області: сумарна помилка і поріг
A = X["Кут нахилу"]; a = A[A["Назва"] == "Сумська область"]
fig, ax1 = plt.subplots(figsize=(5.2, 3.3))
ax1.plot(a["Кут нахилу"], a["Сумарна помилка (хибні сигнали + запізнення)"], color="#c00000", marker="o", ms=4, label="сумарна помилка (хибні сигнали + запізнення)")
ax1.set_xlabel("Кут нахилу δ"); ax1.set_ylabel("Сумарна помилка, тижнів на сезон", color="#c00000")
ax2 = ax1.twinx(); ax2.spines["right"].set_visible(True)
ax2.plot(a["Кут нахилу"], a["Епідемічний поріг"], color="#1f77b4", marker="s", ms=4, label="епідемічний поріг")
ax2.set_ylabel("Епідемічний поріг, на 100 тис.", color="#1f77b4")
ch = a[a["Обрано"] == True]
ax1.axvline(float(ch["Кут нахилу"].iloc[0]), color="grey", ls=":", lw=1)
fig.tight_layout(); fig.savefig(f"{FIG}/fig5_angle.png", dpi=220); plt.close(fig)
print("cstar", round(cstar, 2), "saved", os.listdir(FIG))
