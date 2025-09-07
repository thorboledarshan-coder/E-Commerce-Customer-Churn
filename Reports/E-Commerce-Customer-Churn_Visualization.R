library(ggplot2)
library(patchwork)

# 1.Churn Distribution
p1 <- ggplot(churn, aes(x=churn, fill=churn)) +
  geom_bar() +
  labs(title="Customer Churn Distribution", x="Churn", y="Count") +
  theme_minimal()

# 2. Churn by Gender
p2 <- ggplot(churn, aes(x=gender, fill=churn)) +
  geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Churn % by Gender", y="Percentage") +
  theme_minimal()

# 3. Churn by Payment Mode
p3 <- ggplot(churn, aes(x=preferred_payment_mode, fill=churn)) +
  geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Churn % by Payment Mode", y="Percentage") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1))

# 4. Order Count Distribution: Churned vs Active
p4 <- ggplot(churn, aes(x=churn, y=order_count, fill=churn)) +
  geom_boxplot() +
  labs(title="Order Count Distribution: Churned vs Active") +
  theme_minimal()

# Print plots
print(p1)
print(p2)
print(p3)
print(p4)
#combines images in one frame
(p1 | p2 )/
  (p3 | p4)
(p1)/
  (p2)/
  (p3)/
  (p4)