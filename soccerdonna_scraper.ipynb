import requests
from bs4 import BeautifulSoup
import pandas as pd
import time

BASE_URL = "https://www.soccerdonna.de"
WSL_URL = f"{BASE_URL}/en/womens-super-league/startseite/wettbewerb_ENG1.html"
HEADERS = {"User-Agent": "Mozilla/5.0"}

# Step 1: Get all club profile URLs and their verein IDs
def get_club_urls():
    resp = requests.get(WSL_URL, headers=HEADERS)
    soup = BeautifulSoup(resp.content, 'html.parser')
    club_links = {}

    for row in soup.select("table.standard_tabelle tr.hell"):
        a = row.select_one("td:nth-of-type(2) a")
        if a:
            name = a.get_text(strip=True)
            href = a.get("href", "")
            if "/verein_" in href:
                full_url = BASE_URL + href
                club_links[name] = full_url
    return club_links

# Step 2: Scrape each club squad to get player profile links
def get_player_urls(club_url):
    player_urls = []
    resp = requests.get(club_url, headers=HEADERS)
    soup = BeautifulSoup(resp.content, 'html.parser')
    for a in soup.select("table#spieler a.fb"):
        href = a.get("href", "")
        if "/profil/spieler_" in href:
            full_url = BASE_URL + href
            player_urls.append(full_url)
    return player_urls

# Step 3: Scrape each player profile
def scrape_player_profile(player_url, club_name):
    data = {"Club": club_name, "Profile URL": player_url}
    resp = requests.get(player_url, headers=HEADERS)
    soup = BeautifulSoup(resp.content, 'html.parser')

    h1 = soup.select_one(".tabelle_spieler h1")
    data["Name"] = h1.get_text(strip=True) if h1 else None

    for tr in soup.select(".tabelle_grafik .tabelle_spieler tr"):
        tds = tr.select("td")
        if len(tds) == 2:
            key = tds[0].get_text(strip=True).strip(":")
            value = tds[1].get_text(strip=True, separator=" ")
            data[key] = value

    return data

# Run the scraper
def main():
    clubs = get_club_urls()
    print(f"Found {len(clubs)} clubs")

    all_players = []

    for club_name, club_url in clubs.items():
        print(f"\n🔍 Scraping club: {club_name}")
        player_urls = get_player_urls(club_url)
        print(f"  - Found {len(player_urls)} players")

        for p_url in player_urls:
            print(f"    ↳ {p_url}")
            try:
                player_data = scrape_player_profile(p_url, club_name)
                all_players.append(player_data)
                time.sleep(0.5)  # Be polite
            except Exception as e:
                print(f"Failed to scrape {p_url}: {e}")

    # Save the result
    df = pd.DataFrame(all_players)
    df.to_csv("wsl_player_data.csv", index=False)
    print("\n✅ Done. Data saved to wsl_player_data.csv")

if __name__ == "__main__":
    main()
