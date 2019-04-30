-- import scarf dhall definitions at the top
-- then, you'll specify a list of distributions (one per platform)
-- `URI` can point to a local tar archive or a remote one
let platforms =
	  https://gist.githubusercontent.com/aviaviavi/16caf330e97df23c892cab1c97316ba9/raw

in  { name =
		"confetti"
	, author =
		"Avi Press"
	, copyright =
		"2019 Avi Press"
	, license =
		"MIT"
	, version =
		"1.0.1"
	, distributions =
		[ { platform =
			  platforms.mac
		  , simpleExecutableInstall =
			  [ "confetti" ] : Optional Text
		  , uri =
			  "https://github.com/aviaviavi/confetti/releases/download/1.0.1/confetti-1.0.1-mac.tar.gz"
		  }
		, { platform =
			  platforms.linux_x86_64
		  , simpleExecutableInstall =
			  [ "confetti" ] : Optional Text
		  , uri =
			  "https://github.com/aviaviavi/confetti/releases/download/1.0.1/confetti-1.0.1-linux.tar.gz"
		  }
		]
	}
