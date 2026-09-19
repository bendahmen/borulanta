-- Capture only the document we open, after its fixture list has loaded.
-- Leave it open so the user can inspect the page after the sync.
on run argv
  set leagueURL to item 1 of argv
  tell application "Safari"
    activate
    set leagueDocument to make new document with properties {URL:leagueURL}
  end tell
  set deadline to (current date) + 300
  repeat while (current date) < deadline
    tell application "Safari"
      if not (exists leagueDocument) then error "The league tab was closed. Run the sync again."
      set loadedURL to URL of leagueDocument
      if loadedURL is leagueURL or loadedURL is (leagueURL & "/") then
        try
          set pageHTML to do JavaScript "document.readyState === 'complete' && document.querySelector('.section-fixtures .accordion-leagues') ? document.documentElement.outerHTML : ''" in leagueDocument
        on error errorMessage number errorNumber
          error "Safari could not capture the page. Enable Safari > Settings > Advanced > Show features for web developers, then Develop > Allow JavaScript from Apple Events, and run the sync again. Details: " & errorMessage number errorNumber
        end try
        if pageHTML is not missing value and pageHTML is not "" then return pageHTML
      end if
    end tell
    delay 1
  end repeat
  error "Timed out waiting for league fixtures. Complete the bot check in Safari and run the sync again."
end run
