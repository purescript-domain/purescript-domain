module Domain.Homepage.SupportButtons
  ( UseSupportButtons
  , css
  , useSupportButtons
  ) where

import Prelude

import Domain.Homepage.SupportButton (UseSupportButton, useSupportButton)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, Pure)
import Halogen.Hooks as Hooks
import Tecton (CSS, display, em, gap, inlineFlex, universal, (:=), (?))
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule
import Web.HTML.Common (ClassName(..))

block = ClassName "support-buttons" :: ClassName

buttonWrapElement = ClassName "support-buttons__button-wrap" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    display := inlineFlex
    gap := em 0.5

foreign import data UseSupportButtons :: HookType

instance
  HookNewtype UseSupportButtons
    (UseSupportButton <> UseSupportButton <> UseSupportButton <> Pure)

useSupportButtons :: forall m w i. Hook m UseSupportButtons (HH.HTML w i)
useSupportButtons =
  Hooks.wrap Hooks.do
    tweet <-
      useSupportButton
        { url: "https://twitter.com/intent/tweet?url=https%3A%2F%2Fpurescri.pt"
        , imageURL:
            "data:image/svg+xml,%0A%3Csvg viewBox='283 396 171 140' xmlns='http://www.w3.org/2000/svg'%3E%3Cpath d='m 453.82593,412.80619 c -6.3097,2.79897 -13.09189,4.68982 -20.20852,5.54049 7.26413,-4.35454 12.84406,-11.24992 15.47067,-19.46675 -6.79934,4.03295 -14.3293,6.96055 -22.34461,8.53841 -6.41775,-6.83879 -15.56243,-11.111 -25.68298,-11.111 -19.43159,0 -35.18696,15.75365 -35.18696,35.18525 0,2.75781 0.31128,5.44359 0.91155,8.01875 -29.24344,-1.46723 -55.16995,-15.47582 -72.52461,-36.76396 -3.02879,5.19662 -4.76443,11.24048 -4.76443,17.6891 0,12.20777 6.21194,22.97747 15.65332,29.28716 -5.76773,-0.18265 -11.19331,-1.76565 -15.93716,-4.40083 -0.004,0.14663 -0.004,0.29412 -0.004,0.44248 0,17.04767 12.12889,31.26806 28.22555,34.50266 -2.95247,0.80436 -6.06101,1.23398 -9.26989,1.23398 -2.2673,0 -4.47114,-0.22124 -6.62011,-0.63114 4.47801,13.97857 17.47214,24.15143 32.86992,24.43441 -12.04227,9.43796 -27.21366,15.06335 -43.69965,15.06335 -2.84014,0 -5.64082,-0.16722 -8.39349,-0.49223 15.57186,9.98421 34.06703,15.8094 53.93768,15.8094 64.72024,0 100.11301,-53.61524 100.11301,-100.11387 0,-1.52554 -0.0343,-3.04251 -0.10204,-4.55261 6.87394,-4.95995 12.83891,-11.15646 17.55618,-18.21305 z' /%3E%3C/svg%3E%0A"
        , label: "Share"
        }
    star <-
      useSupportButton
        { url: "https://github.com/purescript-domain/purescript-domain"
        , imageURL:
            "data:image/svg+xml,%0A%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'%3E%3Cpath d='M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z'/%3E%3C/svg%3E%0A"
        , label: "Star"
        }
    sponsor <-
      useSupportButton
        { url: "https://github.com/sponsors/purescript-domain"
        , imageURL:
            "data:image/svg+xml,%0A%3Csvg viewBox='0 0 1000 1000' xmlns='http://www.w3.org/2000/svg'%3E%3Cg%3E%3Cg transform='translate(0.000000,511.000000) scale(0.100000,-0.100000)'%3E%3Cpath d='M2565.1,4604.3c-586.5-82.2-1155.3-368.8-1584.1-799.8C345.6,3169.1,32.3,2289.2,112.3,1362.8c71.1-806.5,351-1548.5,879.8-2335c793.2-1179.7,2195.1-2401.7,3783.6-3297l226.6-128.9l228.8,128.9c693.2,391,1388.6,870.9,2044,1413c351,291,1108.7,1050.9,1357.5,1361.9c402.1,504.3,735.4,1050.9,930.9,1533c593.2,1450.8,395.5,2832.7-539.9,3765.8c-395.5,397.7-866.5,651-1421.9,770.9c-277.7,57.8-826.5,53.3-1106.4-11.1c-506.6-117.7-955.3-359.9-1330.8-722.1l-162.2-155.5L4829,3853.3c-362.1,346.6-833.1,599.9-1315.3,708.7C3296,4610.9,2778.3,4635.4,2565.1,4604.3z'/%3E%3C/g%3E%3C/g%3E%3C/svg%3E%0A"
        , label: "Sponsor"
        }
    Hooks.pure
      $ HH.div
          [ HP.class_ block ]
      $ HH.div [ HP.class_ buttonWrapElement ] <<< pure <$>
          [ tweet, star, sponsor ]
