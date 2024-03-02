#!/usr/bin/env bash

set -euo pipefail

changed_files="$(git diff --name-only origin/main)"
if [[ -z "${changed_files}" ]]; then
    exit 0
fi

commit_messages="$(git log origin/main..)"
if tr '[:upper:]' '[:lower:]' <<< "${commit_messages}" | \
        tr '\n' ' ' | sed -E 's/[[:space:]]+/ /g' | \
        grep -q "no changelog update needed"; then
    exit 0
fi

if ! grep -qF CHANGELOG.md <<< "${changed_files}"; then
    cat <<"EOF"
<== lint-changelog ==>

Please update the changelog to cover the changes you made. Or, if the
changes don't need to be documented in the changelog, add the text "no
changelog update needed" to one of your commit messages. Line breaks
and case sensitivity do not matter.

Remember, when writing a changelog entry, the idea is a user can use
it to understand what differences they might notice after upgrading to
the new version. So, include enough context for someone who isn't
working directly on the code to understand. If user-visible behavior
hasn't changed since the last release, for example because you're
fixing a bug that was just introduced, then you don't need a changelog
entry.

<== lint-changelog ==>

lint-changelog: Please update the changelog to cover the changes you made.
EOF
    exit 1
fi
