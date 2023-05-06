module Domain.Homepage.CSS
  ( css
  ) where

import Prelude

import Domain.Homepage.App as App
import Domain.Homepage.Article as Article
import Domain.Homepage.Footer as Footer
import Domain.Homepage.Header as Header
import Domain.Homepage.Home as Home
import Domain.Homepage.Link as Link
import Domain.Homepage.Logo as Logo
import Domain.Homepage.Markdown as Markdown
import Domain.Homepage.ResponsiveRow as ResponsiveRow
import Domain.Homepage.SearchBox as SearchBox
import Domain.Homepage.SupportButton as SupportButton
import Domain.Homepage.SupportButtons as SupportButtons
import Tecton (pretty, renderSheet)

css :: String
css =
  "html,body{box-sizing:border-box}" <>
    renderSheet pretty do
      App.css
      Article.css
      Footer.css
      Header.css
      Home.css
      Logo.css
      Link.css
      Markdown.css
      ResponsiveRow.css
      SearchBox.css
      SupportButton.css
      SupportButtons.css