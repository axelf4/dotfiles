function New-Symlink {
    Param($symlink, $target)

    if (Test-Path $target -pathType container) {
        # Remove-Item cannot be used to remove folder symlinks,
        # because it also removes the target folder.
        if (Test-Path $symlink) { cmd /c rmdir /s /q $symlink }
        (cmd /c mklink /d $symlink $target) > $null
    }
    else {
        if (Test-Path $symlink) { Remove-Item $symlink }
        (cmd /c mklink $symlink $target) > $null
    }

    Write-Host "$symlink -> $target"
}

Write-Host "Creating symbolic links..."
$files = Get-ChildItem
foreach ($file in $files) {
	$name = $file.FullName | Resolve-Path -Relative
	$symlink = "$HOME\$name"
	$target = $file.FullName
	New-Symlink "$symlink" "$target"
}
Write-Host "Done!"
