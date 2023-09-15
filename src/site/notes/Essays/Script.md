---
dg-publish: true
---

```python
import os
import google_auth_oauthlib.flow
import googleapiclient.discovery
import googleapiclient.errors

scopes = ["https://www.googleapis.com/auth/youtube.readonly"]

def removeChars(title):
    return title.replace("\'","").replace("\"","").replace("|","-").replace("/","-").replace("?","").replace(":"," -").replace(".","").replace("–","-").replace("#","-").replace("[","(").replace("]",")").replace("{","(").replace("}",")").replace("’","").replace("“","").replace("”","").replace("‘","").replace("é","e").replace("è","e").replace("ä","a").replace("ò","o").replace("ù","u").replace("ç","c").replace("*","X")

def main():
    # Disable OAuthlib's HTTPS verification when running locally.
    # *DO NOT* leave this option enabled in production.
    os.environ["OAUTHLIB_INSECURE_TRANSPORT"] = "1"
    links=[]
    titles=[]
    api_service_name = "youtube"
    api_version = "v3"
    client_secrets_file = "YOUR_CLIENT_SECRET_FILE.json"
    
    # Create a directory to store the Markdown files (if it doesn't exist)
    output_dir = 'essays'
    os.makedirs(output_dir, exist_ok=True)

    # Get credentials and create an API client
    flow = google_auth_oauthlib.flow.InstalledAppFlow.from_client_secrets_file(
        client_secrets_file, scopes)
    credentials = flow.run_console()
    youtube = googleapiclient.discovery.build(
        api_service_name, api_version, credentials=credentials)

    # Get the first page of the playlist
    request = youtube.playlistItems().list(
        part="snippet",
        maxResults=50,
        playlistId="YOUR_PLAYLIST_ID"
    )
    response = request.execute()
    anchorItems = response.get("items")
    for anchorItem in anchorItems:
        link = anchorItem["snippet"]["resourceId"]["videoId"]
        links.append("https://www.youtube.com/embed/"+link)
        title = removeChars(anchorItem["snippet"]["title"])
        titles.append(title)

    # From second page to last page use token
    nextPageToken = response.get("nextPageToken")
    while nextPageToken:
        request = youtube.playlistItems().list(
            part="snippet",
            maxResults=50,
            pageToken=nextPageToken,
            playlistId="PLRCUiiyKB7fHIqtAr5fAK4HWZxjexqo1-"
        )
        response = request.execute()
        anchorItems = response.get("items")
        for anchorItem in anchorItems:
            link = anchorItem["snippet"]["resourceId"]["videoId"]
            links.append("https://www.youtube.com/embed/"+link)
            title = removeChars(anchorItem["snippet"]["title"])
            titles.append(title)
        nextPageToken = response.get("nextPageToken")

    # Write the title to the index page 
    # Create a page and an iframe for each video
    with open("Essays/Essays.md", "w") as f1:
        f1.write("---\ndg-publish: true\n---")
        for i in range(len(links)):
            f1.write("\n- [["+titles[i]+"]]")
            with open("Essays/"+titles[i]+".md", "w") as f:
                f.write(
                    "---\ndg-publish: true\n---\n"+
                    '<iframe src="'
                    + links[i] +
                    '" allow="fullscreen" allowfullscreen="" style="height:100%;width:100%; aspect-ratio: 16 / 9; "></iframe>'
                )
            print(i, "ok", titles[i])
    print("Done")

if __name__ == "__main__":
    main()
```
You can download the secret file after the creation on the google api website
```json
{
    "installed": {
        "client_id": "YOUR_CLIENT_ID.apps.googleusercontent.com",
        "project_id": "YOUR_PROJECT_ID",
        "auth_uri": "https://accounts.google.com/o/oauth2/auth",
        "token_uri": "https://oauth2.googleapis.com/token",
        "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
        "client_secret": "YOUR_CLIENT_SECRET",
        "redirect_uris": [
            "http://localhost"
        ]
    }
}
```