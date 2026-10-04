package ai4p.doingstuff

import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

import site.given

import ai4p.doingstuff.widgets.*


val mcpDeck = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# Doing stuff...
      |
      |The world is text
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
      |## LLMs are great at language
      |
      |To be useful for more than chat, an LLM-powered app needs to be able to do things:
      |
      |* read/write files
      |* query a database
      |* search the web
      |* control a robot arm, a spreadsheet, a drawing pad...
      |
      |However, large amounts of computing have been about text, so the same techniques can be used to teach
      |LLMs to act in the world.
      |
      |We can see this even just looking at how this slide deck was built
      |
      |""".stripMargin
  )
  .imageSlide("Minimax code from an earlier deck", "images/minimaxcode.png")
  .imageSlide("Instructions to build a VM, in a Dockerfile", "images/dockerfile.png")
  .imageSlide("Log output from part of the build", "images/buildlog.png")
  .imageSlide("Making a web request to GitHub for my user profile", "images/curltogithubapi.png")
  .markdownSlides(
    """
      |## Getting the LLM to act...
      |
      |LLMs produce text tokens, so on the LLM side, it's a matter of producing text where it is clear where the LLM wishes to make a tool call
      |
      |```
      |I will check the weather for you. <tool_call name="get_weather"><parameter name="location">Armidale, NSW</parameter><parameter name="unit">celsius</parameter></tool_call>
      |```
      |
      |Once the text has been generated, infrastructure code can alter this to be easier for applications to work with. For example, Claude communicates over its API
      |using JSON so this might become:
      |
      |<pre style="overflow-y: scroll; height=260px;">
      |{
      |  "id": "msg_01X7893456789",
      |  "type": "message",
      |  "role": "assistant",
      |  "model": "claude-3-5-sonnet-20241022",
      |  "content": [
      |    {
      |      "type": "text",
      |      "text": "I will check the weather for you."
      |    },
      |    {
      |      "type": "tool_use",
      |      "id": "toolu_01A2B3C4D5E6",
      |      "name": "get_weather",
      |      "input": {
      |        "location": "Armidale, NSW",
      |        "unit": "celsius"
      |      }
      |    }
      |  ],
      |  "stop_reason": "tool_use",
      |  "stop_sequence": null,
      |  "usage": {
      |    "input_tokens": 420,
      |    "output_tokens": 95
      |  }
      |}
      |</pre>
      |
      |
      |""".stripMargin
  )
  .imageSlide("Claude code testing code it generated", "images/claudecodelog.png")
  .markdownSlide(
    """
      |## Claude's generated tmp test code
      |
      |Because LLMs can generate code, they can generate programs that do stuff. In deciding to
      |test a simple widget, Claude started a server that simulated a browser, took a screenshot
      |in a tmp directory, and took that screenshot in as a result
      |
      |<pre>
      |import { chromium } from 'playwright';
      |
      |const browser = await chromium.launch();
      |const page = await browser.newPage({ viewport: { width: 1400, height: 900 } });
      |const errors = [];
      |page.on('console', msg => { if (msg.type() === 'error') errors.push(msg.text()); });
      |page.on('pageerror', err => errors.push('pageerror: ' + err.message));
      |
      |await page.goto('http://localhost:5173/ai4p/#/decks/embeddingsDeck/17', { waitUntil: 'networkidle' });
      |await page.waitForTimeout(1000);
      |
      |const text0 = await page.locator('.rr-text').first().textContent();
      |console.log('initial (layers=1):', text0);
      |await page.locator('.rr-text').first().scrollIntoViewIfNeeded();
      |await page.screenshot({ path: '/tmp/pw-rr/initial.png' });
      |
      |// drag slider to max (12)
      |await page.evaluate(() => {
      |  const input = document.querySelector('.rr-controls input[type=range]');
      |  input.value = 12;
      |  input.dispatchEvent(new Event('input', { bubbles: true }));
      |});
      |await page.waitForTimeout(200);
      |const text12 = await page.locator('.rr-text').first().textContent();
      |console.log('at layers=12:', text12);
      |await page.screenshot({ path: '/tmp/pw-rr/twelve.png' });
      |
      |// set to 5
      |await page.evaluate(() => {
      |  const input = document.querySelector('.rr-controls input[type=range]');
      |  input.value = 5;
      |  input.dispatchEvent(new Event('input', { bubbles: true }));
      |});
      |await page.waitForTimeout(200);
      |const text5 = await page.locator('.rr-text').first().textContent();
      |console.log('at layers=5:', text5);
      |const countLabel = await page.locator('.rr-count').first().textContent();
      |console.log('count label:', countLabel);
      |
      |console.log('ERRORS:', errors.length);
      |errors.forEach(e => console.log(' -', e));
      |await browser.close();
      |</pre>
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Model Context Protocol (MCP) architecture"),
    markdown.div(
      """|
         |MCP is probably the most well-known protocol for LLMs to act in the world. 
         |It is a way to connect LLMs to tools that you (or others) decide to host. 
         |
         |Essentially, an LLM would need to know
         |
         |* That the tool exists (and it can call it) 
         |* How to call it
         |
         |A host application holds an MCP client for every server it's connected to. 
         |
         |Each server is an independent program that can listen for requests -- it's an API
         |
         |The prompt to the LLM tells it what tools are available, and the host application handles making those calls to the MCP
         |""".stripMargin
    ),
    <.p(McpArchitectureDiagram.render),
  ))
  .markdownSlide(
    """
      |## Under the hood: JSON-RPC 2.0
      |
      |The client asks what's on offer:
      |
      |```json
      |{"jsonrpc": "2.0", "id": 1, "method": "tools/list"}
      |```
      |
      |The server replies with a menu, e.g.:
      |
      |```json
      |{"result": {"tools": [
      |  {"name": "move_right",
      |   "description": "Slide the pen right by `steps` units.",
      |   "inputSchema": {"type": "object",
      |                    "properties": {"steps": {"type": "number"}},
      |                    "required": ["steps"]}}
      |]}}
      |```
      |
      |If the model decides to use a tool, the client sends:
      |
      |```json
      |{"jsonrpc": "2.0", "id": 2, "method": "tools/call",
      | "params": {"name": "move_right", "arguments": {"steps": 40}}}
      |```
      |
      |...and feeds the result back to the model.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    markdown.div(
      """
        |## Let's fake one, live
        |
        |There's no server behind this slide deck, but let's simulate a tool that an LLM could call.
        |
        |This one's trivial: an API to draw shapes using vertical and horizontal lines, a
        |bit like an Etch-a-Sketch.
        |
        |""".stripMargin
    ),
    <.p(McpDrawDemo()),
  ))
  .markdownSlide(
    """
      |## Why MCP is popular -
      |
      |Because the tool-description format and transport are standardised, one server implementation
      |works with *any* MCP client. There are now thousands of community and official MCP servers,
      |e.g.
      |
      |* Filesystem, Git, GitHub, Slack, Google Drive
      |* Postgres, SQLite and other databases
      |* Browser automation (Playwright), Blender, Figma
      |* Your own internal APIs
      |
      |The "network effect" means MCP is likely to be supported for a long time: 
      |
      |* Tool-providers want to support MCP because so many LLMs support it
      |* LLM-providers want to support MCP because so many tools support it
      |
      |""".stripMargin
  )
  .markdownSlide(willCcBy)
  .renderSlides
