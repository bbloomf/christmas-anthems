\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 2)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #6
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key g\major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
}

sopMusic = \relative c' {
  \partial 4 g'4 |
  g d' d8[ c] |
  b4~ b b8[ a] |
  g4 b a8[ g] |
  g2 \bar""

  g4 |
  g d' d8[ c] |
  b4 b b8[ a] |
  g4 b a8[ g] |
  g2 \bar""

  \repeat unfold 2 {
    b4 |
    a4 a c |
    b g g |
    b d cis |
    d2 \bar""

    b4 |
    c8.[ d16] e8[ d] c4 |
    d d b8[ a] |
    g4 b a8[ g] |
  }
  \alternative {
    {g2\bar""\break}
    {g2.\bar"|."}
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  A Vir -- gin most pure, the Pro -- phet fore -- told,
  Should bring forth a Sav -- iour which now we be -- hold,
  To be our Re -- deem -- er from Death, Hell, and Sin,
  Which A -- dam’s Trans -- gres -- sion in -- vol -- ved us in.

  Then let us be Mer -- ry, cast Sor -- row a -- way,
  Our Sav -- iour Christ Je -- sus was born on this Day.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Through Beth -- le -- hem \set ignoreMelismata = ##t Ci -- ty, \unset ignoreMelismata in Ju -- ry it was,
  That Jo -- seph and Ma -- ry to -- geth -- er did pass;
  And for to be Tax -- ed with Ma -- ry Straight -- ways,
  Old Cæ -- sar com -- mand -- ed, he quick -- ly o -- beys.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  But Ma -- ry’s full \set ignoreMelismata = ##t Time be -- \unset ignoreMelismata ing come as we find,
  She brought forth her First -- born to save all Man -- kind;
  The Inn be -- ing full; for this Heav -- en -- ly Guest,
  No place there was found where to lay him to rest.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  But Ma -- ry, blest \set ignoreMelismata = ##t Ma -- ry, \unset ignoreMelismata so meek and so mild,
  Soon wrapt up in Swad -- lings this Heav -- en -- ly Child;
  Con -- tent -- ed, she laid him where Ox -- en did feed,
  The great God of Na -- ture ap -- prov’d of the Deed.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  To teach us Hu -- \set ignoreMelismata = ##t mil -- i -- \unset ignoreMelismata ty, all this was done,
  Then learn we from hence haugh -- ty Pride for to shun;
  A Man -- ger’s his Cra -- dle, who came from A -- bove,
  The great God of Mer -- cy, of Peace, and of Love.
}

sopWordsVI = \lyricmode {
  \set stanza = #"6. "
  The pres -- ent -- ly \set ignoreMelismata = ##t af -- ter \unset ignoreMelismata the Shep -- herds did spy,
  Vast Num -- bers of An -- gels to stand in the Sky;
  So mer -- ri -- ly Talk -- ing, so sweet they did Sing,
  All Glo -- ry and Praise to our Heav -- en -- ly King.
}

altoMusic = \relative c' {
  b4 |
  d d fis |
  g4~ g d4 |
  e e d |
  b2 \bar""

  b4 |
  d d fis |
  g g d |
  e e d |
  b2 \bar""

  \repeat unfold 2 {
    b4 |
    d d e |
    d b b |
    d d e |
    fis2 \bar""

    g8[ fis] |
    e4 e g |
    fis d d |
    e e d |
  }
  \alternative {
    {b2}
    {b2.}
  }
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \oneVoice
  g,4 |
  g' g d |
  e4~ e g8[ fis] |
  e4 c d |
  g,2 \bar""

  g4 |
  g' g d |
  e e g8[ fis] |
  e4 c d |
  g,2 \bar""

  \repeat unfold 2 {
    g4 |
    d' d c |
    g' g e |
    b' b a |
    d,2 \bar""

    e8[ d] |
    c4 c e |
    d d g8[ fis] |
    e4 c d |
  }
  \alternative {
    {g,2}
    {g2.}
  }
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsVI
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos" \lyricsto "sopranos" \sopWords
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Christmas Carrol"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
  tagline = ""
}}
global = {
  \key g\major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 g'4 |
  d' d e8 fis |
  g2 b,4 |
  c8.[ d16 e8 d c b] |
  a2 b4 |

  \slurDashed d( c) b |
  \slurSolid e( fis) g |
  d( c) b |
  a2 a4 |
  \time 6/4
  g2.~ g2
  \repeat volta 2 {
    d'4 |

    e2 fis4 g2 fis4 |
    e( d) c b2 b4 |
    e,2 b'4 c8([ b] a4) g |
    fis2 \bar""

    b4 c2 d4 |
    e2 d4 e8([ fis] g4) b, |
    a2 d4 e8[ d e fis] g4 |
    b, a4.( g8) g2
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Ho -- san -- na to the high -- est Joy __ be -- tide,
  th’Heav’n -- ly Bride -- groom, and his Ho -- ly Bride.

  Let Heav’n a -- bove be fill’d with Songs, let Earth tri -- umph be -- low,
  For ev -- er si -- lent be __ those Tongues, that can __ be si -- lent __ now.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Be rav -- ish’d Earth to see this Con -- tract driv’n,
  Twixt sin -- ful __ Man and rec -- on ciled Heav’n;
  Dis -- mount you Choirs of An -- gels come,
  With Men your Joys div -- ide;
  Heav’n nev -- er show’d so sweet a Groom,
  Nor Earth __ so fair a __ Bride.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  All Glo -- ry be to God, to Man __ good Will,
  \set ignoreMelismata = ##t God sent \unset ignoreMelismata his Son, his Prom -- ise to ful -- fill; __
  Let all Man -- kind be fill’d with Mirth,
  Their joy -- ful Ac -- cents raise,
  And join with An -- gels at __ his Birth,
  To sing __ a Sav -- iour’s __ Praise.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 |
  g fis g8 a |
  b2 g4 |
  e2 g4 |
  fis2 d4 |

  \slurDashed g( a) d, |
  \slurSolid cis( d) e |
  d( e8[ fis]) g4 |
  fis2 fis4 |
  \time 6/4
  g2.~ g2 g4 |

  g2 d4 d( e) fis |
  g2 a4 g2 d4 |
  e2 fis4 g( a ) b |
  a2 \bar""

  g4 e2 d4 |
  g2 f4 e2 g4 |
  fis2 d4 g8[ a b a] g4 |
  g2 fis4 g2 
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \oneVoice
  g4 |
  b b e,8 d |
  g2 g4 |
  a8.[ b16 c8 b a g] |
  d2 g4 |

  \slurDashed b( a) g8[ fis] |
  \slurSolid e4( d) g |
  b( a) g |
  d2 d4 |
  \time 6/4
  g2.~ g2 g4 |

  c2 d4 g,2 d4 |
  e2 fis4 g2 b,4 |
  c2 d4 e( fis) g |
  d2 \bar""

  g4 a2 b4 |
  c2 b4 a2 g4 |
  d2 b4 c2 c4 |
  d2 d4 g2
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos" \lyricsto "sopranos" \sopWords
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Carrol"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
  tagline = ""
}}
global = {
  \key g\major
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 d4 |
  g fis g8.[ a16 b8. c16]( |
  d4) e d8[ c] b4 |
  a4. a8 g4 \bar""

  b |
  d e d c |
  b a b cis |
  d2. \bar""

  c4 |
  b a e d |
  g b8[ d] b4 g |
  a4 \bar""

  d b a |
  e d g8[ a] b[ c] |
  d2 e8[ d c b] |
  a4. a8 g4 \bar"||"

  g4 |
  b a g b |
  c b a b |
  c e d c |
  b8.[ a16 g8. a16 b8. c16]( d4)~ |
  d2. r4 |
  d8[ e16 d c8 d16 c b8 c16 b a8 b16 a]( |
  d,2.) fis4 |
  g2. \bar"|."
}
sopWordsAbove = \lyricmode {
  \repeat unfold 55 ""
  Notes, __
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Re -- joice, re -- joice, __ ye Mor -- tals all re -- joice,
  And pierce the Sky with a me -- lo -- dious Voice.
  No Song so sweet, no Notes too high to sing.
  The great and match -- less Good -- ness of our __ heav’n -- ly King.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Shall An -- gels sing, __ and shell Men si -- lent be;
  Nor im -- i -- tate their Heav’n -- ly Har -- mo -- ny?
  Rouse up my Soul, de -- clare that on this Morn,
  Un -- to Man -- kind on Earth, is a Mes -- si -- ah born.

  O how can Men for -- bear to sing, when Earth with An -- gels \set associatedVoice = "altos" Notes, __
  Notes, __ Notes __ do ring.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Glo -- ry to God, __ and Joy to all a -- bove,
  The Earth is crown’d with Plen -- ty, Men with Love;
  Thus sung the heav’n -- ly Host, in bright Ar -- ray,
  Good Will to Men, the Prince of Peace is __ born this Day.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  r4 |
  r d d2~ |
  d4 g g d |
  d4. c8 b4 \bar""

  d |
  d g fis e8[ fis] |
  g4 a g e |
  d2. \bar""

  e4 |
  e d b d |
  e fis g b, |
  d4 \bar""

  d e d |
  b d g fis |
  g8[ a g fis] e2 |
  d4. c8 b4 \bar"||"

  d4 |
  d d b d |
  e d d d |
  e g fis e |

  d8.[ c16 b8. c16 d8. e16]( d4) |
  g4( fis a d,) |
  d1~ |

  \times2/3{d8[ e d](} \times2/3{g[) fis g](} a4) fis |
  g2. \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \oneVoice
  d,4\rest |
  d\rest d g8.[ fis16 g8. a16]( |
  d,4) c g' g |
  d4. d8 g,4 \bar""

  g' |
  b e,8[ fis] g4 a |
  g fis g a |
  d,2. \bar""

  c4 |
  e fis g fis |
  e d e g |
  d \bar""

  b e fis |
  g fis e d |
  g8[ fis e d] c[ b a g] |
  d'4. d8 g,4 \bar"||"

  g' |
  g d e g |
  c, g d' g |
  e c d e8[ fis] |

  g2( d |
  g8[ a16 g b8 c16 b a8 b16 a g8 a16 g] |
  d1)~ |
  d2. d4 |
  g2. \bar"|."
}
bassWords = \lyricmode {
  \repeat unfold 53 ""
  Notes __
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Lyrics = "sops"
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos" \lyricsto "sopranos" \sopWords
    \context Lyrics = "sops" \lyricsto "sopranos" \sopWordsAbove
    \new Lyrics = "basses" \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Christmas Hymn"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
  tagline = ""
}}
