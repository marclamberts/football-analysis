# Import libraries
import pandas as pd
import numpy as np
import joblib

# Step 1: Load the trained model
model = joblib.load("sequence_classifier.pkl")

# Step 2: Prepare new sequence data
# Replace this with your new Opta event data
new_data = pd.DataFrame({
    "sequence_id": [1, 1, 1, 2, 2, 2],  # Example sequence IDs
    "event_type": ["pass", "pass", "pass", "pass", "pass", "pass"],  # Example event types
    "start_x": [10, 20, 30, 40, 50, 60],  # Example start x-coordinates
    "start_y": [10, 20, 30, 40, 50, 60],  # Example start y-coordinates
    "end_x": [20, 30, 40, 50, 60, 70],  # Example end x-coordinates
    "end_y": [20, 30, 40, 50, 60, 70],  # Example end y-coordinates
    "timestamp": [1, 2, 3, 1, 2, 3],  # Example timestamps
    "player_x": [10, 20, 30, 40, 50, 60],  # Example player x-coordinates
    "player_y": [10, 20, 30, 40, 50, 60]  # Example player y-coordinates
})

print("New data:")
print(new_data)

# Step 3: Feature Engineering for new data
# Group events by sequence
new_sequences = new_data.groupby("sequence_id")

# Initialize feature list
new_features = []

for seq_id, seq_data in new_sequences:
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
    new_features.append([num_passes, avg_pass_length, progressive_distance, total_pass_distance, directness, sequence_duration, position_variance])

# Convert to DataFrame
feature_columns = ["num_passes", "avg_pass_length", "progressive_distance", "total_pass_distance", "directness", "sequence_duration", "position_variance"]
new_features_df = pd.DataFrame(new_features, columns=feature_columns)

print("\nNew features DataFrame:")
print(new_features_df)

# Step 4: Predict using the trained model
predictions = model.predict(new_features_df)
new_features_df["predicted_label"] = predictions

print("\nPredictions:")
print(new_features_df[["num_passes", "directness", "predicted_label"]])
