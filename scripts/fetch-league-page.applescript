-- Track the new window by ID: Safari document references can become stale
-- when the bot check changes the page's title or navigates.
-- Leave it open so the user can inspect the page after the sync.
on run argv
  set leagueURL to item 1 of argv
  tell application "Safari"
    activate
    make new document with properties {URL:leagueURL}
    set leagueWindowID to id of front window
  end tell
  set deadline to (current date) + 300
  repeat while (current date) < deadline
    tell application "Safari"
      if not (exists window id leagueWindowID) then error "The league window was closed. Run the sync again."
      try
        repeat with leagueTab in tabs of window id leagueWindowID
          set loadedURL to URL of leagueTab
          if loadedURL is leagueURL or loadedURL is (leagueURL & "/") then
            set pageHTML to do JavaScript "document.readyState === 'complete' && document.querySelector('.section-fixtures .accordion-leagues') ? document.documentElement.outerHTML : ''" in leagueTab
            if pageHTML is not missing value and pageHTML is not "" then return pageHTML
          end if
        end repeat
      on error errorMessage number errorNumber
        -- A tab can briefly disappear from the scripting interface during navigation.
        if errorNumber is not -1728 and errorNumber is not -1719 then
          error "Safari could not capture the page. Check Develop > Allow JavaScript from Apple Events. Details: " & errorMessage number errorNumber
        end if
      end try
    end tell
    delay 1
  end repeat
  error "Timed out waiting for league fixtures. Complete the bot check in Safari and run the sync again."
end run
