library(DiagrammeR)

library(DiagrammeR)

mermaid("
graph TD
    A[正态性检验 P = 0.10] --> B{方差齐性？}
    B -->|P > 0.05| C[独立样本 t 检验]
    B -->|P ≤ 0.05| D[Welch 校正 t 检验]
    C --> E[报告效应量：Cohen's d]
    D --> E
    A -->|若怀疑正态性| F[Mann-Whitney U 检验]
")


