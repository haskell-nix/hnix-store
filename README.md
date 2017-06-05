# `nix-derivation 1.0.0`

Use this package to parse and render Nix derivations such as those stored
in `/nix/store/*.drv` files.  For example, if you had the following derivation
saved at
`/nix/store/zzhs4fb83x5ygvjqn5rdpmpnishpdgy6-perl-MIME-Types-2.13.drv`:

```
Derive([("devdoc","/nix/store/15x9ii8c3n5wb5lg80cm8x0yk6zy7rha-perl-MIME-Types-2
.13-devdoc","",""),("out","/nix/store/93d75ghjyibmbxgfzwhh4b5zwsxzs44w-perl-MIME
-Types-2.13","","")],[("/nix/store/57h2hjsdkdiwbzilcjqkn46138n1xb4a-perl-5.22.3.
drv",["out"]),("/nix/store/cvdbbvnvg131bz9bwyyk97jpq1crclqr-MIME-Types-2.13.tar.
gz.drv",["out"]),("/nix/store/p5g31bc5x92awghx9dlm065d7j773l0r-stdenv.drv",["out
"]),("/nix/store/x50y5qihwsn0lfjhrf1s81b5hgb9w632-bash-4.4-p5.drv",["out"])],["/
nix/store/cdips4lakfk1qbf1x68fq18wnn3r5r14-builder.sh"],"x86_64-linux","/nix/sto
re/fi3mbd2ml4pbgzyasrlnp0wyy6qi48fh-bash-4.4-p5/bin/bash",["-e","/nix/store/cdip
s4lakfk1qbf1x68fq18wnn3r5r14-builder.sh"],[("AUTOMATED_TESTING","1"),("PERL_AUTO
INSTALL","--skipdeps"),("buildInputs",""),("builder","/nix/store/fi3mbd2ml4pbgzy
asrlnp0wyy6qi48fh-bash-4.4-p5/bin/bash"),("checkTarget","test"),("devdoc","/nix/
store/15x9ii8c3n5wb5lg80cm8x0yk6zy7rha-perl-MIME-Types-2.13-devdoc"),("doCheck",
"1"),("installTargets","pure_install"),("name","perl-MIME-Types-2.13"),("nativeB
uildInputs","/nix/store/nsa311yg8h93wfaacjk16c96a98bs09f-perl-5.22.3"),("out","/
nix/store/93d75ghjyibmbxgfzwhh4b5zwsxzs44w-perl-MIME-Types-2.13"),("outputs","ou
t devdoc"),("propagatedBuildInputs",""),("propagatedNativeBuildInputs",""),("src
","/nix/store/5smhymz7viq8p47mc3jgyvqd003ab732-MIME-Types-2.13.tar.gz"),("stdenv
","/nix/store/s3rlr45jzlzx0d6k2azlpxa5zwzr7xyy-stdenv"),("system","x86_64-linux"
)])
```

... you could parse that derivation using:

```
>>> text <- Data.Text.Lazy.IO.readFile "/nix/store/zzhs4fb83x5ygvjqn5rdpmpnishpdgy6-perl-MIME-Types-2.13.drv"
>>> let result = Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation text
>>> result
Done "" (Derivation {outputs = fromList [("devdoc",DerivationOutput {path = File
Path "/nix/store/15x9ii8c3n5wb5lg80cm8x0yk6zy7rha-perl-MIME-Types-2.13-devdoc", 
hashAlgo = "", hash = ""}),("out",DerivationOutput {path = FilePath "/nix/store/
93d75ghjyibmbxgfzwhh4b5zwsxzs44w-perl-MIME-Types-2.13", hashAlgo = "", hash = ""
})], inputDrvs = fromList [(FilePath "/nix/store/57h2hjsdkdiwbzilcjqkn46138n1xb4
a-perl-5.22.3.drv",fromList ["out"]),(FilePath "/nix/store/cvdbbvnvg131bz9bwyyk9
7jpq1crclqr-MIME-Types-2.13.tar.gz.drv",fromList ["out"]),(FilePath "/nix/store/
p5g31bc5x92awghx9dlm065d7j773l0r-stdenv.drv",fromList ["out"]),(FilePath "/nix/s
tore/x50y5qihwsn0lfjhrf1s81b5hgb9w632-bash-4.4-p5.drv",fromList ["out"])], input
Srcs = fromList [FilePath "/nix/store/cdips4lakfk1qbf1x68fq18wnn3r5r14-builder.s
h"], platform = "x86_64-linux", builder = FilePath "/nix/store/fi3mbd2ml4pbgzyas
rlnp0wyy6qi48fh-bash-4.4-p5/bin/bash", args = ["-e","/nix/store/cdips4lakfk1qbf1
x68fq18wnn3r5r14-builder.sh"], env = fromList [("AUTOMATED_TESTING","1"),("PERL_
AUTOINSTALL","--skipdeps"),("buildInputs",""),("builder","/nix/store/fi3mbd2ml4p
bgzyasrlnp0wyy6qi48fh-bash-4.4-p5/bin/bash"),("checkTarget","test"),("devdoc","/
nix/store/15x9ii8c3n5wb5lg80cm8x0yk6zy7rha-perl-MIME-Types-2.13-devdoc"),("doChe
ck","1"),("installTargets","pure_install"),("name","perl-MIME-Types-2.13"),("nat
iveBuildInputs","/nix/store/nsa311yg8h93wfaacjk16c96a98bs09f-perl-5.22.3"),("out
","/nix/store/93d75ghjyibmbxgfzwhh4b5zwsxzs44w-perl-MIME-Types-2.13"),("outputs"
,"out devdoc"),("propagatedBuildInputs",""),("propagatedNativeBuildInputs",""),(
"src","/nix/store/5smhymz7viq8p47mc3jgyvqd003ab732-MIME-Types-2.13.tar.gz"),("st
denv","/nix/store/s3rlr45jzlzx0d6k2azlpxa5zwzr7xyy-stdenv"),("system","x86_64-li
nux")]})
```

... and render the result back to the original derivation:

```
>>> fmap buildDerivation result
Done "" "Derive([(\"devdoc\",\"/nix/store/15x9ii8c3n5wb5lg80cm8x0yk6zy7rha-perl-
MIME-Types-2.13-devdoc\",\"\",\"\"),(\"out\",\"/nix/store/93d75ghjyibmbxgfzwhh4b
5zwsxzs44w-perl-MIME-Types-2.13\",\"\",\"\")],[(\"/nix/store/57h2hjsdkdiwbzilcjq
kn46138n1xb4a-perl-5.22.3.drv\",[\"out\"]),(\"/nix/store/cvdbbvnvg131bz9bwyyk97j
pq1crclqr-MIME-Types-2.13.tar.gz.drv\",[\"out\"]),(\"/nix/store/p5g31bc5x92awghx
9dlm065d7j773l0r-stdenv.drv\",[\"out\"]),(\"/nix/store/x50y5qihwsn0lfjhrf1s81b5h
gb9w632-bash-4.4-p5.drv\",[\"out\"])],[\"/nix/store/cdips4lakfk1qbf1x68fq18wnn3r
5r14-builder.sh\"],\"x86_64-linux\",\"/nix/store/fi3mbd2ml4pbgzyasrlnp0wyy6qi48f
h-bash-4.4-p5/bin/bash\",[\"-e\",\"/nix/store/cdips4lakfk1qbf1x68fq18wnn3r5r14-b
uilder.sh\"],[(\"AUTOMATED_TESTING\",\"1\"),(\"PERL_AUTOINSTALL\",\"--skipdeps\"
),(\"buildInputs\",\"\"),(\"builder\",\"/nix/store/fi3mbd2ml4pbgzyasrlnp0wyy6qi4
8fh-bash-4.4-p5/bin/bash\"),(\"checkTarget\",\"test\"),(\"devdoc\",\"/nix/store/
15x9ii8c3n5wb5lg80cm8x0yk6zy7rha-perl-MIME-Types-2.13-devdoc\"),(\"doCheck\",\"1
\"),(\"installTargets\",\"pure_install\"),(\"name\",\"perl-MIME-Types-2.13\"),(\
"nativeBuildInputs\",\"/nix/store/nsa311yg8h93wfaacjk16c96a98bs09f-perl-5.22.3\"
),(\"out\",\"/nix/store/93d75ghjyibmbxgfzwhh4b5zwsxzs44w-perl-MIME-Types-2.13\")
,(\"outputs\",\"out devdoc\"),(\"propagatedBuildInputs\",\"\"),(\"propagatedNati
veBuildInputs\",\"\"),(\"src\",\"/nix/store/5smhymz7viq8p47mc3jgyvqd003ab732-MIM
E-Types-2.13.tar.gz\"),(\"stdenv\",\"/nix/store/s3rlr45jzlzx0d6k2azlpxa5zwzr7xyy
-stdenv\"),(\"system\",\"x86_64-linux\")])"
```

## Development status

[![Build Status](https://travis-ci.org/Gabriel439/Haskell-Nix-Derivation-Library.png)](https://travis-ci.org/Gabriel439/Haskell-Nix-Derivation-Library)

If you would like to add support for additional functionality, just open an
issue or pull request

## License (BSD 3-clause)

    Copyright (c) 2017 Gabriel Gonzalez
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without modification,
    are permitted provided that the following conditions are met:
        * Redistributions of source code must retain the above copyright notice,
          this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above copyright notice,
          this list of conditions and the following disclaimer in the documentation
          and/or other materials provided with the distribution.
        * Neither the name of Gabriel Gonzalez nor the names of other contributors
          may be used to endorse or promote products derived from this software
          without specific prior written permission.
    
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
