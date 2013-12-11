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
  first-page-number = #8
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
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  b'1 b2 |
  d1 d2 |
  b1 g2 |
  fis1. |
  g1. \bar"||"
  b1\rest
  \repeat volta 2 {
    b2 |
    b1 b2 |

    b1 a4( b) |
    c2( b) a |
    a1 \bar"||"
    b2 |
    a1 a2 |
    b1 d2 |
    b2.( a4) g2 |
    fis1 \bar"||"

    a2 |
    g1 d'2 |
    b1 g2 |
    g1 fis2 |
    g1
  }
}
sopWords = \lyricmode {
  Ho -- ly, ho -- ly, ho -- ly Lord God,
  All Things de -- clare thy Ma -- jes -- ty,
  An -- gels and Men still cry __ a -- loud,
  Glo -- ry to Thee, O \set associatedVoice = "altos" God, most high.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d1 g2 |
  fis1 d2 |
  d1 d2 |
  d1. |
  b1. \bar"||"
  s1
  g'2 |
  g1 g2 |

  g1 fis2 |
  e1 e2 |
  fis1 \bar"||"
  g2 |
  fis1 d2 |
  d( e) d |
  g( fis) e |
  <d \tweak #'font-size #-2 a'>1 \bar"||"

  d2 |
  g1 a2 |
  g1 d2 |
  e2 d2.( c4) |
  b1
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
  g1 g2 |
  a1 a2 |
  b1 b2 |
  a4.( b8 c4 b a4. g8) |
  g1. \bar"||"
  s1 b4( c) |
  d1 d2 |

  d1 d2 |
  e( d) cis |
  d1 \bar"||"
  d2 |
  d1 d,2 |
  g1 g2 |
  b4.( c8 d2) c4( b) |
  a1 \bar"||"

  d4( c) |
  b1 a2 |
  b1 d2 |
  c b( a) |
  g1
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g1 g2 |
  d1 d2 |
  g1 g2 |
  d1. |
  g, \bar"||"
  d'1\rest g2 |
  g1 g2 |

  g1 a4( g) |
  fis2( g) a |
  d,1 \bar"||"
  g2 |
  d1 d2 |
  b1 b2 |
  g2.( a4) b( c) |
  d1 \bar"||"

  d2 |
  e1 fis2 |
  g1 b,2 |
  c2 d1 |
  g,1
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"An Hymn to be sung between the Epistle and Gospel"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
  tagline = ""
}}
global = {
  \key c\major
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 2 e'4( d) |
  c2( b) a |
  g1 c2 |
  d1 c2 |
  b1 \bar"||"
  c2 |
  d1 c2 |
  b1 c2 |
  a1 b2 |
  c1 \bar"||"

  c2 |
  a1 a2 |
  b1 c2 |
  c1 c2 |
  d1 \bar"||"
  e4( d) |
  c1 b2 |
  a1 g2 |
  c1 b2 |
  c1 \bar"||"

  e4( d) |
  c1 d2 |
  b1 a2 |
  g( c) b |
  c1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Im -- mor -- tal babe, who on this day,
  Didst change thy heav’ns for our vile clay,
  And didst with flesh thy god -- head veil:
  E -- ter -- nal son of God all hail!
  E -- ter -- nal son of God all hail!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Bright An -- gels ush -- er’d in thy Birth,
  With Ac -- cla -- ma -- tions and with Mirth;
  And sung a -- loud, that for our Sake,
  Thou didst our hu -- man Na -- ture take,
  Thou didst our hu -- man Na -- ture take.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  And shall not we, poor Crea -- tures, whom
  Thou didst from Heav’n on Pur -- pose come,
  To save, when Slaves to Sa -- tan, we
  Knew Nought but Sin and Mis -- e -- ry,
  Knew Nought but Sin and Mis -- e -- ry.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  We all thro’ Ig -- no -- rance and Vice,
  Had lost our Way to Pa -- ra -- dise;
  And had with -- out God in the World,
  Been in -- to out -- er Dark -- ness hurl’d,
  Been in -- to out -- er Dark -- ness hurl’d.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  Had not that Day -- Spring from on high,
  Vis -- it -- ed Mor -- tals far and nigh;
  And Christ, our true E -- man -- u -- el,
  Stopp’d us in our Ca -- reer to Hell,
  Stopp’d us in our Ca -- reer to Hell.
}

sopWordsVI = \lyricmode {
  \set stanza = #"6. "
  Rav -- ish’d, Lord, with this Love of thine,
  To sing thy Praise our Souls in -- cline;
  And that we may per -- form our Parts,
  We’ll to our Voi -- ces tune our Hearts,
  We’ll to our Voi -- ces tune our Hearts.
}

altoMusic = \relative c' {
  g'4( fis) |
  e1 d2 |
  c1 a'2 |
  g1 fis2 |
  g1 \bar"||"
  g2 |
  g1 g2 |
  g1 g2 |
  e1 e2 |
  f1 \bar"||"

  f2 |
  a1 fis2 |
  g1 g2 |
  e1 d2 |
  d1 \bar"||"
  g4( f) |
  e1 d2 |
  c1 g'2 |
  g1 g2 |
  g1 \bar"||"

  g2 |
  a1 fis2 |
  g1 a2 |
  g1 f2 |
  e1 \bar"|."
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
  c2 |
  e,1 f2 |
  g1 c2 |
  b2 c1 |
  d1 \bar"||"
  e2 |
  f1 e2 |
  d1 c4( b) |
  c2 b2.( a4) |
  a1 \bar"||"

  c2 |
  c1 c2 |
  b1 a2 |
  c2 b( a) |
  g1 \bar"||"
  g2 |
  a1 b2 |
  c1 d2 |
  f e( d) |
  c1 \bar"||"

  c2 |
  a1 d2 |
  d1 e2 |
  f e( d) |
  c1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  c,2 |
  c1 d2 |
  e1 f2 |
  g1 a2 |
  g1 \bar"||"
  c2 |
  b1 c2 |
  g1 g2 |
  a1 a2 |
  f1 \bar"||"

  a2 |
  a1 a2 |
  g1 g2 |
  a1 g4( f) |
  g1 \bar"||"
  e2 |
  f1 g2 |
  a1 b2 |
  c1 g2 |
  c1 \bar"||"

  e,2 |
  f1 d2 |
  g1 a2 |
  c1 g2 |
  c,1 \bar"|."
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
    \new Lyrics = "altosVI"  \lyricsto "sopranos" \sopWordsVI
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"An Hymn for Christmas Day"}}
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
  g'2 r |
  b r |
  d e4 fis |
  g4. b,8 c4 b |
  a2 \repeat volta 2 {
    r4 d |

    b g r e' |
    c a r d |
    e fis g b, |
    a2. a4
  }
  \alternative {
    {\partial 2 g2}
    {g1 \bar"||"}
  }

  d'2 d4 d |
  e2 cis |
  d4 c! b2 |
  b a4 g |
  d2 \bar""

  d' |
  d4 cis d2 |
  r d |
  e4 fis g b, |
  a2. a4 |
  g1 \bar"|."
}
sopWords = \lyricmode {
  \set associatedVoice = "altos" Hark, \unset associatedVoice Hark,
  \set associatedVoice = "altos" hark, \unset associatedVoice hark, hark, hark, what News the An -- gels \set associatedVoice = "altos" bring,
  \unset associatedVoice Glad _ \set associatedVoice = "altos" Tid -- ings,
  \unset associatedVoice glad _ \set associatedVoice = "basses" Tid -- ings,
  \unset associatedVoice glad _ Tid -- ings of a New born King,
  King.

  Born of a Maid, a Vir -- gin pure, born with -- out Sin,
  from Guilt se -- \set associatedVoice = "altos" cure, \unset associatedVoice born _ with -- out Sin, from Guilt se -- cure.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  r2 d |
  r g |
  fis e4 d |
  g4. fis8 e[ fis] g4 |
  fis2 d |

  d4 d g2 |
  e4 e r2 |
  e4 d e g |
  fis2. fis4
  g2
  g1 \bar"||"

  g2 fis4 d |
  g2 e |
  a4 fis g2 |
  d e4 e |
  d2 \bar""

  d |
  g4 a fis2 |
  g2 r |
  e4 d e g |
  fis2. fis4 |
  g1 \bar"|."
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
  d,2\rest d |
  d\rest g |
  d' c4 b |
  e,4( fis8) g a4 g |
  d2 d |

  g4 g e2 |
  a4 a g2 |
  c4 b a g |
  d2. d4
  g2
  g1 \bar"||"

  g2 b4 b |
  e,2 a |
  fis4 d g2 |
  g c,4 c |
  d2 \bar""

  g |
  b4 a d,2 |
  d\rest g |
  c4 b a g |
  d2. d4 |
  <g g,>1 \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Christmas Hymn"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
  tagline = ""
}}
