curl -s https://news.ycombinator.com > now.html
./run < now.html > now.out
./tab.sh now.out
