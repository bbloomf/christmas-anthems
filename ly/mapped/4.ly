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
  first-page-number = #4
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
  \key c\major
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 2 c'2 |
  e1 d2 |
  d1 b2 |
  c1 b2 |
  c1 \bar"||"
  b2 |
  c( d) e |

  d1 a2 |
  b1 \bar"||:"\break
  \repeat volta 2 {
    g2 |
    c1 c2 |
    c( d) e |
    c( b) a |
    b1 \bar"||"

    %page(16)
    b2 |
    c( b c |
    d1) b2 |
    c1.~ |
    c2( d) e |

    c( b) a |
    b4.( a8 g2) g |
    g1
  }
  \break
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set associatedVoice = "altos" 
  What Words! what Voic -- es can we bring?
  Which Way our Ac -- cents raise,
  To wel -- come the __ mis -- te -- rious King.
  And sing, __ and sing, __ and sing __ a Sav -- iour’s Praise.

  \set stanza = #"5. "
  Long let the u -- ni -- ver -- sal Frame
  The great Re -- deem -- er __ sing,
  And Men and An -- gels at __ the Name
  Bow to, __ bow to, __ bow to __ the mys -- tic King.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set associatedVoice = "altos" 
  What earth -- ly Har -- mo -- ny can reach
  Up to a Theme so __ high,
  When An -- gels ne’er __ cou’d soar __ that Pitch,
  Who dwell, __ who dwell, __ who dwell a -- bove the Sky?

  \set stanza = #"6. "
  Re -- demp -- tion be the gen -- ’ral Sound,
  This Day no Grief ap -- pear,
  From Earth to Heav’n the Notes re -- bound,
  And Mer -- cy, Mer -- cy, Mer -- cy smiled to hear.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set associatedVoice = "altos" 
  Lo! Heav’n this Day de -- scends to Earth,
  Th’Im -- mor -- tal mor -- tal __ grows,
  Made Man by this __ stu -- pen -- dious Birth
  To quell, __ to quell, __ to quell our dead -- ly Foes.

  \set stanza = #"7. "
  Oh! ’tis too lit -- tle all we can
  For this un -- bound -- ed __ Love,
  All that was ev -- er writ __ by Man,
  Or sung, __ or sung, __ or sung in Hymns a -- bove.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set associatedVoice = "altos"
  In Swad -- ling bands the God -- head lies
  To hu -- man Flesh de -- based,
  That we, His dear -- ly ran -- som’d Prize,
  Might be, __ might be, __ might be __ to Glo -- ry rais’d.

  \set stanza = #"8. "
  But tho’ we can’t fit Lan -- guage find,
  We Praise, Be -- lieve, A -- dore!
  With joy -- ful Hearts, and Souls re -- sign’d,
  And wish, __ and wish, __ and wish we could do more!
}

sopWordsV = \lyricmode {
}

sopWordsVI = \lyricmode {
}

sopWordsVII = \lyricmode {
}

sopWordsVIII = \lyricmode {
}

altoMusic = \relative c' {
  e2 |
  g1 a2 |
  g1 g2 |
  g1 f2 |
  e1 \bar"||"
  g2 |
  g( f) e |

  a g( fis) |
  g1 \bar"||:"
  g2 |
  e1 e2 |
  e( f) g |
  a( g) fis |
  g1 \bar"||"

  g2 |
  g( f e |
  g1) g2 |
  a1.( |
  g2 f) e |

  a( g) e |
  g1 g2 |
  e1 
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
  g2 |
  c1 d2 |
  b1 g2 |
  c4.( d8 e2) d |
  c1 \bar"||"
  d2 |
  e( d) c4( b) |

  a2 a2.( g4) |
  g1 \bar"||:"
  b2 |
  c( b) a |
  g( f) e |
  a( b) c |
  d1 \bar"||"

  d2 |
  e( d c4 d8[ c] |
  b4 c8[ b] a2) g |
  c( b a4 b8[ a] |
  g4 a8[ g] f2) c |

  a'2( b) c |
  d b1 |
  c1 
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  c,2 |
  c4.( d8 e2) f |
  g1 g2 |
  e4.( d8 c2) g |
  c1 \bar"||"
  g'2 |
  c( b) c |

  d1 d,2 |
  g1 \bar"||:"
  g2 |
  a( g) f |
  e( d) c |
  f( g) a |
  g1 \bar"||"

  g2 |
  c( g a4 b8[ a] |
  g4 a8[ g] f2) e |
  a( g f4 g8[ f] |
  e4 f8[ e] d2) c |

  f( g) a |
  g1 g,2 |
  c1
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
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold 2 \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold 2 \altoMusic >> }
    >>
    \new Lyrics = "altos"
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "altos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "altos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "altos" \sopWordsV
    \new Lyrics = "altosV"  \lyricsto "altos" \sopWordsVI
    \new Lyrics = "altosV"  \lyricsto "altos" \sopWordsVII
    \new Lyrics = "altosV"  \lyricsto "altos" \sopWordsVIII
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold 2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold 2 \bassMusic >> }
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
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  b'4 a g |
  d'2 d8[ c] |
  b([ a] g4) a8[ b] |
  c2 c4 |
  b2. \bar"||"

  fis4 g8[ a] b[ c] |
  d2 b4 |
  c8([ b] a4) a |
  a2. \bar"||"

  d4 d d |
  a( b) c |
  d2 d4 |
  d8[ c b c] d[ g,] |
  fis2. \bar"||"
  b8[ a] g[ fis] e4 |
  b'8.[ c16 b8 a] g4 |
  c8.([ d16] e4) d |
  b2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Hark! hear you not a cheer -- ful, cheer -- ful Noise,
  which makes the Heav -- ens ring with Joys.

  See where light Stars bright An -- gels, An -- gels fly,
  a Thou -- sand Heav’n -- ly Ech -- oes cry.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  So loud they sung, that down to, down to Earth,
  in -- no cent Chil -- dren heard their Mirth;

  And sung with them what none could, none could say,
  for Joy their Prince was born that Day.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Their Prince, their God, like one of, one of those,
  is made a Child, and wrapt in Cloaths;

  All this in Time was ful -- ly, ful -- ly done,
  we have a Sav -- iour, God, the Son.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'4 fis e |
  d2 d4 |
  g2 fis4 |
  g8([ fis] e4) d |
  g2. \bar"||"

  d4 e8[ fis] g4 |
  a8.([ g16] fis4) g |
  e e2 |
  d2. \bar"||"

  d8[ e] fis4 g |
  a( g) fis |
  g8([ fis] e4) d |
  g8[ a b a] g4 |
  fis8.([ e16] d2) \bar"||"

  d4 e8[ fis] g[ a] |
  g2 d4 |
  g8[ b a g] fis4 |
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
  r2. |
  d4 g, a |
  b( c) d |
  e8[ d c b] a[ g] |
  g2. \bar"||"

  a8[ b] c4 b |
  a( d) e |
  a,( d) cis |
  d2. \bar"||"

  r2. |
  d4 g c, |
  b( c) d |
  b8[ a g a] b[ c] |
  d2. \bar"||"

  b4 c8[ d] e[ d] |
  d2 g8[ fis] |
  e[ d c b] a[ g] |
  g2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 d e8[ fis] |
  g2 fis4 |
  g8([ fis] e4) d |
  c2 d4 |
  g,2. \bar"||"

  d'4 e8[ fis] g4 |
  d2 g4 |
  a2 a,4 |
  d2. \bar"||"

  b'8[ c] d4 g, |
  fis4( g) a |
  d,( e) fis |
  g8[ fis e fis] g[ a] |
  d,2. \bar"||"

  g8[ fis] e[ d] c[ b] |
  g'8.[ a16 g8 fis] e[ d] |
  c2 d4 |
  g,2. \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"An Hymn for Christmas Day"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
  tagline = ""
}}
