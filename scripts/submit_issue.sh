#!/bin/bash

set -e

if [ -z "$1" ]; then
  echo "Usage: $0 <path_to_issue_file>"
  exit 1
fi
path="$1"

if [ ! -f "$path" ]; then
  echo "Error: File '$path' does not exist."
  exit 1
fi

title=$(head -n 1 "$path" | sed 's/^# //')
workstream=$(cat "$path" | awk -e '/^\*\*Workstream:\*\* / {print $0}' | sed 's/^\*\*Workstream:\*\* //')
context=$(cat "$path" | awk -e '/^\*\*Context:\*\* / {print $0}' | sed 's/^\*\*Context:\*\* //')
state=$(cat "$path" | awk -e '/^\*\*State:\*\* / {print $0}' | sed 's/^\*\*State:\*\* //')
requires=$(cat "$path" | 
  awk '
  /^\*\*Requires:/ {
      while (match($0, /\[[0-9]+(\.[0-9]+)*\]/)) {
          printf "%s%s", sep, substr($0, RSTART+1, RLENGTH-2)
          sep=","
          $0 = substr($0, RSTART+RLENGTH)
      }
      print ""
  }' \
  )

declare -a blocked_by=()
for r in ${requires//,/ }; do
    number=$(gh issue list --state All --search "$r in:title" --json number --jq '.[0].number')
    if [ -z "$number" ]; then
        continue
    fi
    blocked_by+=("$number")
done

if [ ${#blocked_by[@]} -ne 0 ]; then
  blocked_by_refs=$(printf "#%s " "${blocked_by[@]}")
fi

headers=$(\
  echo "Title: $title" && \
  echo "Workstream: $workstream" && \
  echo "Context: $context" && \
  echo "State: $state" && \
  [ -n "$blocked_by_refs" ] && echo "Blocked by: $blocked_by_refs" || echo "" \
)

gum style \
  --border rounded \
  --padding "0 2" "$headers"

body=$( \
echo "## Workstream: $workstream" && \
echo "## Context: $context" && \
echo "" && \
cat "$path" | awk -e '\
BEGIN { in_body = 0 } \
/^## / { in_body = 1 } \
in_body { print } \
')

gum style \
  --border rounded \
  --padding "0 2" "$body"

choice=$(
  gum choose \
    --header "Select issue type:" \
    --selected "Feature Request" \
    "Bug" \
    "Feature Request" \
    "Documentation" \
    "Other"
)

if ! gum confirm "Do you want to submit this issue?" --default=false ; then
  echo "Issue submission canceled."
  exit 0
fi

blocked_by_args=$(IFS=,; echo "${blocked_by[*]}")
if [ -n "$blocked_by_args" ]; then
    blocked_by_options="--blocked-by $blocked_by_args"
else
    blocked_by_options=""
fi

gh issue create $blocked_by_options \
    --title "$title" \
    --body "$body"
