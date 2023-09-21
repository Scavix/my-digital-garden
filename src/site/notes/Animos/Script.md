---
{"dg-publish":true,"permalink":"/animos/script/","noteIcon":""}
---

```python
import xml.etree.ElementTree as ET
import os

def removeChars(title):
    return title.replace("\'","").replace("\"","").replace("|","-").replace("/","-").replace("?","").replace(":"," -").replace(".","").replace("–","-").replace("#","-").replace("[","(").replace("]",")").replace("{","(").replace("}",")").replace("’","").replace("“","").replace("”","").replace("‘","").replace("é","e").replace("è","e").replace("ä","a").replace("ò","o").replace("ù","u").replace("ç","c").replace("*","X")

def main():
    # Define the XML file path and parse it
    XML_file = 'animelist.xml'
    tree  = ET.parse(XML_file)
    root = tree.getroot()

    # Create a directory to store the Markdown files (if it doesn't exist)
    output_dir = 'Animos'
    os.makedirs(output_dir, exist_ok=True)
    baseUrl = "https://myanimelist.net/anime/"

    # Write the Index content to a file
    with open(os.path.join(output_dir, "Anime Index.md"), 'w', encoding='utf-8') as f1:
        f1.write("---\ndg-publish: true\n---")
        # Read the parsed XML and generate Markdown files
        for anime in root.findall('anime'):
            series_animedb_id = anime.find('series_animedb_id').text
            series_title = removeChars(anime.find('series_title').text)
            my_score = anime.find('my_score').text
            f1.write("\n- [["+series_title+"]]")
            md_filename = f"{series_title}.md"

            # Construct the Markdown content
            markdown_content = '---\ndg-publish: true\n---\n'
            markdown_content += 'My Score: '+my_score+'\n'
            markdown_content += '[[Animos]]\n'
            markdown_content += '<iframe src=' + baseUrl + series_animedb_id + 'allow="fullscreen" allowfullscreen="" style="height:100%;width:100%; aspect-ratio: 16 / 9; "></iframe>'

            # Write the Markdown content to a file
            with open(os.path.join(output_dir, md_filename), 'w', encoding='utf-8') as mdfile:
                mdfile.write(markdown_content)

    print("Markdown files generated successfully.")

if __name__ == "__main__":
    main()
```