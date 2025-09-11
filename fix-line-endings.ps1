$content = Get-Content "contracts/medical-records-consent.clar" -Raw
$content = $content -replace "`r`n", "`n"
$content = $content -replace "`r", "`n"
[System.IO.File]::WriteAllText("contracts/medical-records-consent.clar", $content, [System.Text.UTF8Encoding]::new($false))
