#!/bin/sh

ARGC_MIN=1
if [[ $# -lt ${ARGC_MIN} ]]
then
    echo "Not enough arguments." 1>&2
    exit 1
fi

repo_path=$1

mkdir -v "${repo_path}" || exit 1

cd "${repo_path}"

git init
echo "This file has no use." > useless.txt
git add useless.txt
git commit -m "Added useless.txt"

echo "{}" > authors.json
echo "{\"screen_sizes\":[\"small\",\"large\"]}" > blog.json
echo "This file will never change." > static.txt

git add authors.json blog.json static.txt
git commit -m "Added authors.json and blog.json."

echo "[{\"first_name\":\"Science\",\"last_name\":\"Writer\",\"nominal_id\":\"swriter\",\"avatars\":[{\"timestamp\":100,\"images\":[\"swriter_small.jpg\",\"swriter_large.jpg\"]}]},{\"first_name\":\"Politics\",\"last_name\":\"Columnist\",\"nominal_id\":\"pcolumnist\",\"avatars\":[{\"timestamp\":100000,\"images\":[\"pcolumnist_small_old.jpg\",\"pcolumnist_large_old.jpg\"]},{\"timestamp\":120000,\"images\":[\"pcolumnist_small_new.jpg\",\"pcolumnist_large_new.jpg\"]}]}]" > authors.json

echo "* Priorities :blog_post:" > posts.org
echo ":PROPERTIES:" >> posts.org
echo ":nominal_id: priorities" >> posts.org
echo ":date_published: <2026-01-01 12:00>" >> posts.org
echo ":author: swriter" >> posts.org
echo ":subtitle: Making sure everything works." >> posts.org
echo ":END:" >> posts.org
echo "This is a post." >> posts.org

git rm useless.txt

git add authors.json
git add posts.org
git commit -m "Filled authors.json with actual authors, and added posts.org"
