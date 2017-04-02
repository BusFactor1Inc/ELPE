#line 1 "./src/wysiwyg.c"
/*
** Copyright (c) 2012 D. Richard Hipp
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the Simplified BSD License (also
** known as the "2-Clause License" or "FreeBSD License".)
**
** This program is distributed in the hope that it will be useful,
** but without any warranty; without even the implied warranty of
** merchantability or fitness for a particular purpose.
**
** Author contact information:
**   drh@hwaci.com
**   http://www.hwaci.com/drh/
**
*******************************************************************************
**
** This file contains code that generates WYSIWYG text editors on
** web pages.
*/
#include "config.h"
#include <assert.h>
#include <ctype.h>
#include "wysiwyg.h"


/*
** Output code for a WYSIWYG editor.  The caller must have already generated
** the <form> that will contain the editor, and the call must generate the
** corresponding </form> after this routine returns.  The caller must include
** an onsubmit= attribute on the <form> element that invokes the
** wysiwygSubmit() function.
**
** There can only be a single WYSIWYG editor per frame.
*/
void wysiwygEditor(
  const char *zId,        /* ID for this editor */
  const char *zContent,   /* Initial content (HTML) */
  int w, int h            /* Initial width and height */
){

  cgi_printf("<style type=\"text/css\">\n"
         ".intLink { cursor: pointer; }\n"
         "img.intLink { border: 0; }\n"
         "#wysiwygBox {\n"
         "  border: 1px #000000 solid;\n"
         "  padding: 12px;\n"
         "}\n"
         "#editMode label { cursor: pointer; }\n"
         "</style>\n");

  cgi_printf("<input id=\"wysiwygValue\" type=\"hidden\" name=\"%s\">\n"
         "<div id=\"editModeDiv\">Edit mode:\n"
         "  <select id=\"editMode\" size=1 onchange=\"setDocMode(this.selectedIndex)\">\n"
         "<option value=\"0\">WYSIWYG</option>\n"
         "<option value=\"1\">Raw HTML</option>\n"
         "</select></div>\n"
         "<div id=\"toolBar1\">\n"
         "<select onchange=\"formatDoc('formatblock',this[this.selectedIndex].value);\n"
         "                  this.selectedIndex=0;\">\n"
         "<option selected>- formatting -</option>\n"
         "<option value=\"h1\">Title 1 &lt;h1&gt;</option>\n"
         "<option value=\"h2\">Title 2 &lt;h2&gt;</option>\n"
         "<option value=\"h3\">Title 3 &lt;h3&gt;</option>\n"
         "<option value=\"h4\">Title 4 &lt;h4&gt;</option>\n"
         "<option value=\"h5\">Title 5 &lt;h5&gt;</option>\n"
         "<option value=\"h6\">Subtitle &lt;h6&gt;</option>\n"
         "<option value=\"p\">Paragraph &lt;p&gt;</option>\n"
         "<option value=\"pre\">Preformatted &lt;pre&gt;</option>\n"
         "</select>\n"
         "<select onchange=\"formatDoc('fontname',this[this.selectedIndex].value);\n"
         "                  this.selectedIndex=0;\">\n"
         "<option class=\"heading\" selected>- font -</option>\n"
         "<option>Arial</option>\n"
         "<option>Arial Black</option>\n"
         "<option>Courier New</option>\n"
         "<option>Times New Roman</option>\n"
         "</select>\n"
         "<select onchange=\"formatDoc('fontsize',this[this.selectedIndex].value);\n"
         "                  this.selectedIndex=0;\">\n"
         "<option class=\"heading\" selected>- size -</option>\n"
         "<option value=\"1\">Very small</option>\n"
         "<option value=\"2\">A bit small</option>\n"
         "<option value=\"3\">Normal</option>\n"
         "<option value=\"4\">Medium-large</option>\n"
         "<option value=\"5\">Big</option>\n"
         "<option value=\"6\">Very big</option>\n"
         "<option value=\"7\">Maximum</option>\n"
         "</select>\n"
         "<select onchange=\"formatDoc('forecolor',this[this.selectedIndex].value);\n"
         "                  this.selectedIndex=0;\">\n"
         "<option class=\"heading\" selected>- color -</option>\n"
         "<option value=\"red\">Red</option>\n"
         "<option value=\"blue\">Blue</option>\n"
         "<option value=\"green\">Green</option>\n"
         "<option value=\"black\">Black</option>\n"
         "</select>\n"
         "</div>\n"
         "<div id=\"toolBar2\">\n"
         "<img class=\"intLink\" title=\"Undo\" onclick=\"formatDoc('undo');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAOMKADljwliE33mOrpGjuYKl8aezxqPD+7\n"
         "/I19DV3NHa7P///////////////////////yH5BAEKAA8ALAAAAAAWABYAAARR8MlJq704680\n"
         "7TkaYeJJBnES4EeUJvIGapWYAC0CsocQ7SDlWJkAkCA6ToMYWIARGQF3mRQVIEjkkSVLIbSfE\n"
         "whdRIH4fh/DZMICe3/C4nBQBADs=\">\n",(zId));

  cgi_printf("<img class=\"intLink\" title=\"Redo\" onclick=\"formatDoc('redo');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAMIHAB1ChDljwl9vj1iE34Kl8aPD+7/I1/\n"
         "///yH5BAEKAAcALAAAAAAWABYAAANKeLrc/jDKSesyphi7SiEgsVXZEATDICqBVJjpqWZt9Na\n"
         "EDNbQK1wCQsxlYnxMAImhyDoFAElJasRRvAZVRqqQXUy7Cgx4TC6bswkAOw==\">\n");

  cgi_printf("<img class=\"intLink\" title=\"Remove formatting\"\n"
         "onclick=\"formatDoc('removeFormat')\"\n"
         "src=\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AA\n"
         "AABGdBTUEAALGPC/xhBQAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAAOxAAADsQBlSsOGwA\n"
         "AAAd0SU1FB9oECQMCKPI8CIIAAAAIdEVYdENvbW1lbnQA9syWvwAAAuhJREFUOMtjYBgFxAB5\n"
         "01ZWBvVaL2nHnlmk6mXCJbF69zU+Hz/9fB5O1lx+bg45qhl8/fYr5it3XrP/YWTUvvvk3VeqG\n"
         "Xz70TvbJy8+Wv39+2/Hz19/mGwjZzuTYjALuoBv9jImaXHeyD3H7kU8fPj2ICML8z92dlbtMz\n"
         "deiG3fco7J08foH1kurkm3E9iw54YvKwuTuom+LPt/BgbWf3//sf37/1/c02cCG1lB8f//f95\n"
         "DZx74MTMzshhoSm6szrQ/a6Ir/Z2RkfEjBxuLYFpDiDi6Af///2ckaHBp7+7wmavP5n76+P2C\n"
         "lrLIYl8H9W36auJCbCxM4szMTJac7Kza////R3H1w2cfWAgafPbqs5g7D95++/P1B4+ECK8tA\n"
         "wMDw/1H7159+/7r7ZcvPz4fOHbzEwMDwx8GBgaGnNatfHZx8zqrJ+4VJBh5CQEGOySEua/v3n\n"
         "7hXmqI8WUGBgYGL3vVG7fuPK3i5GD9/fja7ZsMDAzMG/Ze52mZeSj4yu1XEq/ff7W5dvfVAS1\n"
         "lsXc4Db7z8C3r8p7Qjf///2dnZGxlqJuyr3rPqQd/Hhyu7oSpYWScylDQsd3kzvnH738wMDzj\n"
         "5GBN1VIWW4c3KDon7VOvm7S3paB9u5qsU5/x5KUnlY+eexQbkLNsErK61+++VnAJcfkyMTIwf\n"
         "fj0QwZbJDKjcETs1Y8evyd48toz8y/ffzv//vPP4veffxpX77z6l5JewHPu8MqTDAwMDLzyrj\n"
         "b/mZm0JcT5Lj+89+Ybm6zz95oMh7s4XbygN3Sluq4Mj5K8iKMgP4f0////fv77//8nLy+7MCc\n"
         "XmyYDAwODS9jM9tcvPypd35pne3ljdjvj26+H2dhYpuENikgfvQeXNmSl3tqepxXsqhXPyc66\n"
         "6s+fv1fMdKR3TK72zpix8nTc7bdfhfkEeVbC9KhbK/9iYWHiErbu6MWbY/7//8/4//9/pgOnH\n"
         "6jGVazvFDRtq2VgiBIZrUTIBgCk+ivHvuEKwAAAAABJRU5ErkJggg==\">\n");

  cgi_printf("<img class=\"intLink\" title=\"Bold\" onclick=\"formatDoc('bold');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAID/AMDAwAAAACH5BAEAAAAALAAAAAAWAB\n"
         "YAQAInhI+pa+H9mJy0LhdgtrxzDG5WGFVk6aXqyk6Y9kXvKKNuLbb6zgMFADs=\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Italic\" onclick=\"formatDoc('italic');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAKEDAAAAAF9vj5WIbf///yH5BAEAAAMALA\n"
         "AAAAAWABYAAAIjnI+py+0Po5x0gXvruEKHrF2BB1YiCWgbMFIYpsbyTNd2UwAAOw==\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Underline\" onclick=\"formatDoc('underline');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAKECAAAAAF9vj////////yH5BAEAAAIALA\n"
         "AAAAAWABYAAAIrlI+py+0Po5zUgAsEzvEeL4Ea15EiJJ5PSqJmuwKBEKgxVuXWtun+DwxCCgA\n"
         "7\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Left align\"\n"
         "onclick=\"formatDoc('justifyleft');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAID/AMDAwAAAACH5BAEAAAAALAAAAAAWAB\n"
         "YAQAIghI+py+0Po5y02ouz3jL4D4JMGELkGYxo+qzl4nKyXAAAOw==\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Center align\"\n"
         "onclick=\"formatDoc('justifycenter');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAID/AMDAwAAAACH5BAEAAAAALAAAAAAWAB\n"
         "YAQAIfhI+py+0Po5y02ouz3jL4D4JOGI7kaZ5Bqn4sycVbAQA7\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Right align\"\n"
         "onclick=\"formatDoc('justifyright');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAID/AMDAwAAAACH5BAEAAAAALAAAAAAWAB\n"
         "YAQAIghI+py+0Po5y02ouz3jL4D4JQGDLkGYxouqzl43JyVgAAOw==\" />\n"
         "<img class=\"intLink\" title=\"Numbered list\"\n"
         "onclick=\"formatDoc('insertorderedlist');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAMIGAAAAADljwliE35GjuaezxtHa7P////\n"
         "///yH5BAEAAAcALAAAAAAWABYAAAM2eLrc/jDKSespwjoRFvggCBUBoTFBeq6QIAysQnRHaEO\n"
         "zyaZ07Lu9lUBnC0UGQU1K52s6n5oEADs=\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Dotted list\"\n"
         "onclick=\"formatDoc('insertunorderedlist');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAMIGAAAAAB1ChF9vj1iE33mOrqezxv////\n"
         "///yH5BAEAAAcALAAAAAAWABYAAAMyeLrc/jDKSesppNhGRlBAKIZRERBbqm6YtnbfMY7lud6\n"
         "4UwiuKnigGQliQuWOyKQykgAAOw==\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Quote\"\n"
         "onclick=\"formatDoc('formatblock','blockquote');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAIQXAC1NqjFRjkBgmT9nqUJnsk9xrFJ7u2\n"
         "R9qmKBt1iGzHmOrm6Sz4OXw3Odz4Cl2ZSnw6KxyqO306K63bG70bTB0rDI3bvI4P/////////\n"
         "//////////////////////////yH5BAEKAB8ALAAAAAAWABYAAAVP4CeOZGmeaKqubEs2Cekk\n"
         "ErvEI1zZuOgYFlakECEZFi0GgTGKEBATFmJAVXweVOoKEQgABB9IQDCmrLpjETrQQlhHjINrT\n"
         "q/b7/i8fp8PAQA7\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Delete indentation\"\n"
         "onclick=\"formatDoc('outdent');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAMIHAAAAADljwliE35GjuaezxtDV3NHa7P\n"
         "///yH5BAEAAAcALAAAAAAWABYAAAM2eLrc/jDKCQG9F2i7u8agQgyK1z2EIBil+TWqEMxhMcz\n"
         "sYVJ3e4ahk+sFnAgtxSQDqWw6n5cEADs=\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Add indentation\"\n"
         "onclick=\"formatDoc('indent');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAOMIAAAAADljwl9vj1iE35GjuaezxtDV3N\n"
         "Ha7P///////////////////////////////yH5BAEAAAgALAAAAAAWABYAAAQ7EMlJq704650\n"
         "B/x8gemMpgugwHJNZXodKsO5oqUOgo5KhBwWESyMQsCRDHu9VOyk5TM9zSpFSr9gsJwIAOw==\">\n");

  cgi_printf("<img class=\"intLink\" title=\"Hyperlink\"\n"
         "onclick=\"var sLnk=prompt('Target URL:','');\n"
         "         if(sLnk&&sLnk!=''){formatDoc('createlink',sLnk)}\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAOMKAB1ChDRLY19vj3mOrpGjuaezxrCztb\n"
         "/I19Ha7Pv8/f///////////////////////yH5BAEKAA8ALAAAAAAWABYAAARY8MlJq704682\n"
         "7/2BYIQVhHg9pEgVGIklyDEUBy/RlE4FQF4dCj2AQXAiJQDCWQCAEBwIioEMQBgSAFhDAGghG\n"
         "i9XgHAhMNoSZgJkJei33UESv2+/4vD4TAQA7\" />\n");

#if 0  /* Cut/Copy/Paste requires special browser permissions for security
       ** reasons.  So omit these buttons */
  cgi_printf("<img class=\"intLink\" title=\"Cut\"\n"
         "onclick=\"formatDoc('cut');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAIQSAB1ChBFNsRJTySJYwjljwkxwl19vj1\n"
         "dusYODhl6MnHmOrpqbmpGjuaezxrCztcDCxL/I18rL1P/////////////////////////////\n"
         "//////////////////////////yH5BAEAAB8ALAAAAAAWABYAAAVu4CeOZGmeaKqubDs6TNnE\n"
         "bGNApNG0kbGMi5trwcA9GArXh+FAfBAw5UexUDAQESkRsfhJPwaH4YsEGAAJGisRGAQY7UCC9\n"
         "ZAXBB+74LGCRxIEHwAHdWooDgGJcwpxDisQBQRjIgkDCVlfmZqbmiEAOw==\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Copy\"\n"
         "onclick=\"formatDoc('copy');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAIQcAB1ChBFNsTRLYyJYwjljwl9vj1iE31\n"
         "iGzF6MnHWX9HOdz5GjuYCl2YKl8ZOt4qezxqK63aK/9KPD+7DI3b/I17LM/MrL1MLY9NHa7OP\n"
         "s++bx/Pv8/f///////////////yH5BAEAAB8ALAAAAAAWABYAAAWG4CeOZGmeaKqubOum1SQ/\n"
         "kPVOW749BeVSus2CgrCxHptLBbOQxCSNCCaF1GUqwQbBd0JGJAyGJJiobE+LnCaDcXAaEoxhQ\n"
         "ACgNw0FQx9kP+wmaRgYFBQNeAoGihCAJQsCkJAKOhgXEw8BLQYciooHf5o7EA+kC40qBKkAAA\n"
         "Grpy+wsbKzIiEAOw==\" />\n");

  cgi_printf("<img class=\"intLink\" title=\"Paste\"\n"
         "onclick=\"formatDoc('paste');\"\n"
         "src=\"data:image/gif;base64,R0lGODlhFgAWAIQUAD04KTRLY2tXQF9vj414WZWIbXmOrp\n"
         "qbmpGjudClFaezxsa0cb/I1+3YitHa7PrkIPHvbuPs+/fvrvv8/f/////////////////////\n"
         "//////////////////////////yH5BAEAAB8ALAAAAAAWABYAAAWN4CeOZGmeaKqubGsusPvB\n"
         "SyFJjVDs6nJLB0khR4AkBCmfsCGBQAoCwjF5gwquVykSFbwZE+AwIBV0GhFog2EwIDchjwRiQ\n"
         "o9E2Fx4XD5R+B0DDAEnBXBhBhN2DgwDAQFjJYVhCQYRfgoIDGiQJAWTCQMRiwwMfgicnVcAAA\n"
         "MOaK+bLAOrtLUyt7i5uiUhADs=\" />\n");
#endif

  cgi_printf("</div>\n"
         "<div id=\"wysiwygBox\"\n"
         " style=\"resize:both; overflow:auto; width: %dem; height: %dem;\"\n"
         " contenteditable=\"true\">%s</div>\n"
         "<script>\n"
         "var oDoc;\n"
         "\n"
         "/* Initialize the document editor */\n"
         "function initDoc() {\n"
         "  oDoc = document.getElementById(\"wysiwygBox\");\n"
         "  if (!isWysiwyg()) { setDocMode(true); }\n"
         "}\n"
         "\n"
         "/* Return true if the document editor is in WYSIWYG mode.  Return\n"
         "** false if it is in Markup mode */\n"
         "function isWysiwyg() {\n"
         "  return document.getElementById(\"editMode\").selectedIndex==0;\n"
         "}\n"
         "\n"
         "/* Invoke this routine prior to submitting the HTML content back\n"
         "** to the server */\n"
         "function wysiwygSubmit() {\n"
         "  if(oDoc.style.whiteSpace==\"pre-wrap\"){setDocMode(0);}\n"
         "  document.getElementById(\"wysiwygValue\").value=oDoc.innerHTML;\n"
         "}\n"
         "\n"
         "/* Run the editing command if in WYSIWYG mode */\n"
         "function formatDoc(sCmd, sValue) {\n"
         "  if (isWysiwyg()){\n"
         "    try {\n"
         "      // First, try the W3C draft standard way, which has\n"
         "      // been working on all non-IE browsers for a while.\n"
         "      // It is also supported by IE11 and higher.\n"
         "      document.execCommand(\"styleWithCSS\", false, false);\n"
         "    } catch (e) {\n"
         "      try {\n"
         "        // For IE9 or IE10, this should work.\n"
         "        document.execCommand(\"useCSS\", 0, true);\n"
         "      } catch (e) {\n"
         "        // Ok, that apparently did not work, do nothing.\n"
         "      }\n"
         "    }\n"
         "    document.execCommand(sCmd, false, sValue);\n"
         "    oDoc.focus();\n"
         "  }\n"
         "}\n"
         "\n"
         "/* Change the editing mode.  Convert to markup if the argument\n"
         "** is true and wysiwyg if the argument is false. */\n"
         "function setDocMode(bToMarkup) {\n"
         "  var oContent;\n"
         "  if (bToMarkup) {\n"
         "    /* WYSIWYG -> Markup */\n"
         "    var linebreak = new RegExp(\"</p><p>\",\"ig\");\n"
         "    oContent = document.createTextNode(\n"
         "                 oDoc.innerHTML.replace(linebreak,\"</p>\\n\\n<p>\"));\n"
         "    oDoc.innerHTML = \"\";\n"
         "    oDoc.style.whiteSpace = \"pre-wrap\";\n"
         "    oDoc.appendChild(oContent);\n"
         "    document.getElementById(\"toolBar1\").style.visibility=\"hidden\";\n"
         "    document.getElementById(\"toolBar2\").style.visibility=\"hidden\";\n"
         "  } else {\n"
         "    /* Markup -> WYSIWYG */\n"
         "    if (document.all) {\n"
         "      oDoc.innerHTML = oDoc.innerText;\n"
         "    } else {\n"
         "      oContent = document.createRange();\n"
         "      oContent.selectNodeContents(oDoc.firstChild);\n"
         "      oDoc.innerHTML = oContent.toString();\n"
         "    }\n"
         "    oDoc.style.whiteSpace = \"normal\";\n"
         "    document.getElementById(\"toolBar1\").style.visibility=\"visible\";\n"
         "    document.getElementById(\"toolBar2\").style.visibility=\"visible\";\n"
         "  }\n"
         "  oDoc.focus();\n"
         "}\n"
         "initDoc();\n"
         "</script>\n",(w),(h),(zContent));

}
