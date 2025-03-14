# Import required libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Step 1: Load and Prepare the Data
# Example data (replace with your actual data)
data = {
    'event_type': ['pass', 'dribble', 'pass', 'tackle', 'interception', 'pass', 'dribble', 'shot', 'pressure', 'recovery'],
    'outcome': ['successful', 'unsuccessful', 'incomplete', 'successful', 'successful', 'successful', 'unsuccessful', 'blocked', 'successful', 'successful'],
    'x': [70, 50, 80, 40, 60, 70, 50, 80, 60, 60],
    'y': [40, 30, 20, 60, 50, 40, 30, 20, 50, 50],
    'player_id': [101, 102, 103, 101, 102, 101, 102, 103, 101, 102],
    'team_id': [1, 2, 1, 2, 1, 2, 1, 2, 1, 2]
}

# Convert to DataFrame
data = pd.DataFrame(data)

# Display the first few rows of the data
print("Raw Data:")
print(data.head())

# Step 2: Identify Attacking Sequences
# Assign a sequence_id to each attacking sequence
data['sequence_id'] = data.groupby((data['team_id'] != data['team_id'].shift()).cumsum()).ngroup()

# Step 3: Calculate Expected Threat (xT) and Expected Goals (xG) for Each Event
# Define xT and xG values for each zone on the pitch (example values)
def get_expected_threat(x, y):
    """
    Assign an Expected Threat (xT) value based on the location of the event.
    - Higher xT in the final third, lower in the defensive third.
    """
    if x > 66:  # Final third
        return 0.1
    elif x > 33:  # Middle third
        return 0.05
    else:  # Defensive third
        return 0.01

def get_expected_goals(x, y):
    """
    Assign an Expected Goals (xG) value based on the location of the event.
    - Higher xG closer to the goal, lower farther away.
    """
    distance_to_goal = np.sqrt((x - 100)**2 + (y - 50)**2)
    return np.exp(-0.1 * distance_to_goal)

data['xT'] = data.apply(lambda row: get_expected_threat(row['x'], row['y']), axis=1)
data['xG'] = data.apply(lambda row: get_expected_goals(row['x'], row['y']), axis=1)

# Step 4: Identify Defensive Interventions
# Define defensive actions that disrupt attacks
defensive_actions = ['tackle', 'interception', 'pressure']

# Create a column to mark defensive interventions
data['defensive_intervention'] = data['event_type'].isin(defensive_actions)

# Step 5: Calculate Disruption Value for Each Sequence
def calculate_disruption(sequence):
    """
    Calculate the reduction in xT and xG caused by defensive interventions in a sequence.
    """
    total_xT = sequence['xT'].sum()
    total_xG = sequence['xG'].sum()
    if sequence['defensive_intervention'].any():
        # If there's a defensive intervention, assume the attack is stopped (xT and xG drop to 0)
        return total_xT, total_xG
    else:
        return 0, 0

# Apply the function to each sequence
sequence_disruption = data.groupby('sequence_id').apply(calculate_disruption).reset_index(name='disruption_value')
sequence_disruption[['xT_reduction', 'xG_reduction']] = pd.DataFrame(sequence_disruption['disruption_value'].tolist(), index=sequence_disruption.index)

# Merge disruption values back into the main data
data = data.merge(sequence_disruption[['sequence_id', 'xT_reduction', 'xG_reduction']], on='sequence_id')

# Step 6: Aggregate Disruption Value by Player
# Group by player and calculate total disruption value, xT reduction, and xG reduction
player_disruption = data.groupby('player_id').agg(
    total_disruption=('xT_reduction', 'sum'),
    xT_reduction=('xT_reduction', 'sum'),
    xG_reduction=('xG_reduction', 'sum'),
    defensive_actions=('defensive_intervention', 'sum')
).reset_index()

# Calculate disruption value per action
player_disruption['disruption_per_action'] = player_disruption['total_disruption'] / player_disruption['defensive_actions']

# Sort players by total disruption value
player_disruption = player_disruption.sort_values(by='total_disruption', ascending=False)

# Display top 10 players by disruption value
print("\nTop 10 Players by Disruption Value:")
print(player_disruption.head(10))

# Step 7: Visualize Results
# Bar chart of top players by total disruption value
plt.figure(figsize=(12, 6))
sns.barplot(data=player_disruption.head(10), x='player_id', y='total_disruption', palette='viridis')
plt.title('Top 10 Players by Total Disruption Value')
plt.xlabel('Player ID')
plt.ylabel('Total Disruption Value')
plt.show()

# Heatmap of disruption events by location
plt.figure(figsize=(8, 6))
heatmap_data = data[data['defensive_intervention']]
sns.kdeplot(x=heatmap_data['x'], y=heatmap_data['y'], cmap='Reds', fill=True, thresh=0.1)
plt.title('Heatmap of Defensive Interventions')
plt.xlabel('Pitch Length (x)')
plt.ylabel('Pitch Width (y)')
plt.xlim(0, 100)
plt.ylim(0, 100)
plt.show()
