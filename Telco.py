import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from scipy.stats import mannwhitneyu, wilcoxon
from scipy import stats
import itertools
import scikit_posthocs as sp
from statsmodels.stats.multitest import multipletests

df = pd.read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

df['TotalCharges'] = pd.to_numeric(df['TotalCharges'], errors='coerce')



table = df.groupby('PaymentMethod')['TotalCharges'].agg(
     count='count',
    mean='mean',
    std='std',
    min='min',
    q25=lambda x: x.quantile(0.25),
    median='median',
    q75=lambda x: x.quantile(0.75),
    max='max'
)

print(table,'/n')



plt.figure(figsize=(8,6))

sns.boxplot(x='PaymentMethod', y='MonthlyCharges', data=df, 
            palette='Set2')

plt.title('Monthly Charges Distribution by Payment Method', fontsize=15)
plt.xlabel('Payment Method', fontsize=12)
plt.ylabel('Monthly Charges ($)', fontsize=12)
plt.xticks(rotation=15)
plt.show()



groups = df.groupby('PaymentMethod')['MonthlyCharges']

print('Shapiro_Wilk test results for each payment group\n')
for name, group in groups:
    stat, p = stats.shapiro(group)
    print(f'{name}: p-value {p:.4e}')
print()


grouped_data = [group['MonthlyCharges'] for name, group in df.groupby('PaymentMethod')]

levene_stat, levene_p = stats.levene(*grouped_data)
print(f"Levene's Test: p-value = {levene_p:.4e}")
print()



grouped_data = [group['MonthlyCharges'] for _, group in df.groupby('PaymentMethod')]
group_names = [name for name, _ in df.groupby('PaymentMethod')]

kw_stat, kw_p = stats.kruskal(*grouped_data)

print("Kruskal-Wallis test for MonthlyCharges by PaymentMethod:")
print(f" - H-statistics = {kw_stat:.2f}")
print(f" - p-value = {kw_p:.4e}")

print("\nGroups included in the test:")
for name in group_names:
    print(f" - {name}")
print()


grouped = df.groupby('PaymentMethod')['MonthlyCharges']

group_names = list(grouped.groups.keys())
group_data = [group.values for _, group in grouped]

pairs = list(itertools.combinations(range(len(group_data)), 2))

results = []

for i, j in pairs:
    data1 = group_data[i]
    data2 = group_data[j]

    stat, p = mannwhitneyu(data1, data2, alternative='two-sided')
    
    results.append({
        'Group 1': group_names[i],
        'Group 2': group_names[j],
        'W-stat': stat,
        'p-value': p
    })

m = len(results)  
for res in results:
    res['p-value_adj'] = min(res['p-value'] * m, 1.0)

results_df = pd.DataFrame(results)
print("Post-hoc Wilcoxon Rank-Sum test with Bonferroni correction:")
print(results_df,'/n')



dunn_results = sp.posthoc_dunn(df, val_col='MonthlyCharges', group_col='PaymentMethod', p_adjust='bonferroni')

print("Dunn's test pairwise comparison (p-values):")
print(dunn_results)
