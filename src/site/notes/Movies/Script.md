---
dg-publish: true
---
```python
import csv
import os

def removeChars(title):
    return title.replace("\'","").replace("\"","").replace("|","-").replace("/","-").replace("?","").replace(":"," -").replace(".","").replace("–","-").replace("#","-").replace("[","(").replace("]",")").replace("{","(").replace("}",")").replace("’","").replace("“","").replace("”","").replace("‘","").replace("é","e").replace("è","e").replace("ä","a").replace("ò","o").replace("ù","u").replace("ç","c").replace("*","X")

def main():

    # Define the CSV file path
    csv_file = 'ratings.csv'

    # Create a directory to store the Markdown files (if it doesn't exist)
    output_dir = 'Kinimatografos'
    os.makedirs(output_dir, exist_ok=True)

    # Write the Index content to a file
    with open(os.path.join(output_dir, "Movies Index.md"), 'w', encoding='utf-8') as f1:
        f1.write("---\ndg-publish: true\n---")
        # Read the CSV file and generate Markdown files
        with open(csv_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                movie_title = removeChars(row['Title'])
                f1.write("\n[["+movie_title+"]]")
                md_filename = f"{movie_title}.md"

                # Construct the Markdown content
                markdown_content = "---\ndg-publish: true\n---\n"
                markdown_content += "Directors: [["+row['Directors']+"]]\n"
                markdown_content += "Your Rating: "+row['Your Rating']+"\n"
                markdown_content += "IMDb Rating: "+row['IMDb Rating']

                # Write the Markdown content to a file
                with open(os.path.join(output_dir, md_filename), 'w', encoding='utf-8') as mdfile:
                    mdfile.write(markdown_content)

    print("Markdown files generated successfully.")

if __name__ == "__main__":
    main()
```