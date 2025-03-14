# Import libraries
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, accuracy_score
import matplotlib.pyplot as plt
import joblib

# Step 1: Load Opta event data
# Replace "opta_event_data.csv" with your actual file path
data = pd.read_csv("opta_event_data.csv")

# Example columns: event_type, player_x, player_y, timestamp, sequence_id
print("Loaded data:")
print(data.head())

# Step 2: Feature Engineering
# Group events by sequence
sequences = data.groupby("sequence_id")

# Initialize feature list
features = []

for seq_id, seq_data in sequences:
    # Filter passes only
    passes = seq_data[seq_data["event_type"] == "pass"]
    
    # Skip sequences with fewer than 3 passes
    if len(passes) < 3:
        continue
    
    # Calculate features
    num_passes = len(passes)
    pass_lengths = np.sqrt((passes["end_x"] - passes["start_x"])**2 + (passes["end_y"] - passes["start_y"])**2)
    avg_pass_length = np.mean(pass_lengths)
    progressive_distance = np.sum(np.abs(passes["end_x"] - passes["start_x"]))  # Only horizontal progression
    total_pass_distance = np.sum(pass_lengths)
    directness = progressive_distance / total_pass_distance
    sequence_duration = seq_data["timestamp"].max() - seq_data["timestamp"].min()
    
    # Variance in player positions
    player_positions = seq_data[["player_x", "player_y"]].values
    position_variance = np.var(player_positions, axis=0).mean()
    
    # Append features
    features.append([num_passes, avg_pass_length, progressive_distance, total_pass_distance, directness, sequence_duration, position_variance])

# Convert to DataFrame
feature_columns = ["num_passes", "avg_pass_length", "progressive_distance", "total_pass_distance", "directness", "sequence_duration", "position_variance"]
features_df = pd.DataFrame(features, columns=feature_columns)

print("\nFeatures DataFrame:")
print(features_df.head())

# Step 3: Labeling
# Example: Label sequences based on directness
features_df["label"] = np.where(features_df["directness"] > 0.5, "relationist", "positional")

print("\nLabel distribution:")
print(features_df["label"].value_counts())

# Step 4: Train-Test Split
X = features_df[feature_columns]
y = features_df["label"]
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Step 5: Train Random Forest Classifier
model = RandomForestClassifier(random_state=42)
model.fit(X_train, y_train)

# Evaluate the model
y_pred = model.predict(X_test)
print("\nModel Evaluation:")
print("Accuracy:", accuracy_score(y_test, y_pred))
print(classification_report(y_test, y_pred))

# Step 6: Feature Importance
importances = model.feature_importances_
feature_names = X.columns

# Plot feature importances
plt.barh(feature_names, importances)
plt.xlabel("Feature Importance")
plt.ylabel("Feature")
plt.title("Random Forest Feature Importance")
plt.show()

# Step 7: Visualize a Sequence
def plot_sequence(sequence_id):
    seq_data = data[data["sequence_id"] == sequence_id]
    plt.figure(figsize=(10, 6))
    plt.scatter(seq_data["player_x"], seq_data["player_y"], c=seq_data["timestamp"], cmap="viridis")
    plt.plot(seq_data["player_x"], seq_data["player_y"], linestyle="-", color="gray")
    plt.colorbar(label="Timestamp")
    plt.title(f"Sequence {sequence_id}")
    plt.xlabel("X Coordinate")
    plt.ylabel("Y Coordinate")
    plt.show()

# Example: Plot a sequence
plot_sequence(sequence_id=123)

# Step 8: Save the Model
joblib.dump(model, "sequence_classifier.pkl")
print("\nModel saved as 'sequence_classifier.pkl'.")

# Step 9: Load the Model (Example)
loaded_model = joblib.load("sequence_classifier.pkl")
print("\nModel loaded successfully.")
