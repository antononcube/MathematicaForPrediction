# AI vision via WL

**Version 0.8**

Anton Antonov   
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)   
[RakuForPrediction at WordPress](https://rakuforprediction.wordpress.com)   
[RakuForPrediction-book at GitHub](https://github.com/antononcube/RakuForPrediction-book)    
November 2023   

## Introduction

In the fall of 2023 OpenAI introduced the image vision model ["gpt-4-vision-preview"](https://platform.openai.com/docs/guides/vision), [OAIb1].

The model "gpt-4-vision-preview" represents a significant enhancement to the GPT-4 model, providing developers and AI enthusiasts with a more versatile tool capable of interpreting and narrating images alongside text. This development opens up new possibilities for creative and practical applications of AI in various fields.

For example, consider the following Wolfram Language (WL), developer-centric applications:

- Narration of UML diagrams

- Code generation from narrated (and suitably tweaked) narrations of architecture diagrams and charts

- Generating presentation content draft from slide images

- Extracting information from technical plots

- etc.

A more diverse set of the applications would be:

- Dental X-ray images narration

- Security or baby camera footage narration

    - How many people or cars are seen, etc.

- Transportation trucks content descriptions

    - Wood logs, alligators, boxes, etc.

- Web page visible elements descriptions

    - Top menu, biggest image seen, etc.

- Creation of recommender systems for image collections

    - Based on both image features and image descriptions

- etc.

As a first concrete example, consider the following image that fable-dramatizes the name "Wolfram" (https://i.imgur.com/UIIKK9w.jpg):

```mathematica
RemoveBackground@Import[URL["https://i.imgur.com/UIIKK9wl.jpg"]]
```

![1xg1w9gct6yca](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/1xg1w9gct6yca.png)

Here is its narration:

```mathematica
LLMVisionSynthesize["Describe very concisely the image", "https://i.imgur.com/UIIKK9w.jpg", "MaxTokens" -> 600]

(*"You are looking at a stylized black and white illustration of a wolf and a ram running side by side among a forest setting, with a group of sheep in the background. The image has an oval shape."*)
```

**Remark:** In this notebook Mathematica and Wolfram Language (WL) are used as synonyms.

**Remark:** This notebook is the WL version of the notebook ["AI vision via Raku"](https://community.wolfram.com/groups/-/m/t/3071989), [AA3].

### Ways to use with WL

There are five ways to utilize image interpretation (or vision) services in WL:

- Dedicated Web API functions, [MT1, CWp1]

- LLM synthesizing,  [[AAp1](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/LLMVision.m),  [WRIp1](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/LLMFunctions/)]

- LLM functions, [[AAp1](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/LLMVision.m),  [WRIp1](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/LLMFunctions/)]

- Dedicated notebook cell type, [[AAp2](https://raku.land/zef:antononcube/Jupyter::Chatbook), AAv1]

- Any combinations of the above

In this document are demonstrated the second, third, and fifth. The first one is demonstrated in the Wolfram Community post ["Direct API access to new features of GPT-4 (including vision, DALL-E, and TTS)](https://community.wolfram.com/groups/-/m/t/3062403)" by Marco Thiel, [MT1]. The fourth one is still "under design and consideration."

**Remark:** The model "gpt-4-vision-preview" is given as a ["chat completion model"](https://platform.openai.com/docs/api-reference/chat/create) , therefore, in this document we consider it to be a Large Language Model (LLM).

### Packages and paclets

Here we load WL package used below, [AAp1, AAp2, AAp3]:

```mathematica
Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/LLMVision.m"]
```

**Remark:** The package LLMVision is "temporary" -- It should be made into a Wolfram repository paclet, or (much  better) its functionalities should be included in the "LLMFunctions" framework, [WRIp1].

### Images

Here are the links to all images used in this document:

```mathematica
tblImgs = {{Row[{"Wolf and ram running together in forest"}], Row[{"https://i.imgur.com/UIIKK9w.jpg", ""}]}, {Row[{"LLM", " ", "functionalities", " ", "mind-map", ""}], Row[{"https://i.imgur.com/kcUcWnql.jpg", ""}]}, {Row[{"Single", " ", "sightseer", ""}], Row[{"https://i.imgur.com/LEGfCeql.jpg", ""}]}, {Row[{"Three", " ", "hunters", ""}], Row[{"https://raw.githubusercontent.com/antononcube/Raku-WWW-OpenAI/main/resources/ThreeHunters.jpg", ""}]}, {Row[{"Cyber", " ", "Week", " ", "Spending", " ", "Set", " ", "to", " ", "Hit", " ", "New", " ", "Highs", " ", "in", " ", "2023", ""}], Row[{"https://cdn.statcdn.com/Infographic/images/normal/7045.jpeg", ""}]}};
tblImgs = Map[Append[#[[1 ;; 1]], Hyperlink[#[[-1, 1, 1]]]] &, tblImgs];
TableForm[tblImgs, TableHeadings -> {None, {"Name", "Link"}}] /. {ButtonBox[n_, BaseStyle -> "Hyperlink", ButtonData -> { URL[u_], None}] :> Hyperlink[n, URL[u]]}
```

| Name                                             | Link                                                                                          |
|--------------------------------------------------|-----------------------------------------------------------------------------------------------|
| Wolf and ram running together in forest          | https://i.imgur.com/UIIKK9w.jpg                                                               |
| LLM functionalities mind-map                     | https://i.imgur.com/kcUcWnql.jpg                                                              |
| Single sightseer                                 | https://i.imgur.com/LEGfCeql.jpg                                                              |
| Three hunters                                    | https://raw.githubusercontent.com/antononcube/Raku-WWW-OpenAI/main/resources/ThreeHunters.jpg |
| Cyber Week Spending Set to Hit New Highs in 2023 | https://cdn.statcdn.com/Infographic/images/normal/7045.jpeg                                   |


### Document structure

Here is the structure of the rest of the document:

- **LLM synthesizing**   
... using multiple image specs of different kind. 

- **LLM functions**  
... workflows over technical plots.

- **Dedicated notebook cells**   
... just excuses why they are not programmed yet.

- **Combinations (fairytale generation)**   
... Multi-modal applications for replacing creative types.

- **Conclusions and leftover comments**   
... frustrations untold.

## LLM synthesizing

The simplest way to use the OpenAI's vision service is through the function LLMVisionSynthesize of the package ["LLMVision"](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/LLMVision.m), [AAp1]. (Already demoed in the introduction.)

If the function LLMVisionSynthesize is given a list of images, a textual result corresponding to those images is returned. The argument "images" is a list of image URLs, image file names, or image Base64 representations. (Any combination of those element types can be specified.)

Before demonstrating the vision functionality below we first obtain and show a couple of images.

### Images

Here is a URL of an image: (https://i.imgur.com/LEGfCeql.jpg). Here is the image itself:

```mathematica
Import[URL["https://i.imgur.com/LEGfCeql.jpg"]]
```

![1u02ytqvf7xi9](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/1u02ytqvf7xi9.png)

OpenAI's vision endpoint accepts POST specs that have image URLs or images converted into Base64 strings. When we use the LLMVisionSynthesize function and provide a file name under the "images" argument, the Base64 conversion is automatically applied to that file.

Here is an example of how we apply Base64 conversion to the [image](https://github.com/antononcube/Raku-WWW-OpenAI/blob/main/resources/ThreeHunters.jpg) from a given file path:

```mathematica
img1 = Import[$HomeDirectory <> "/Downloads/ThreeHunters.jpg"];
ColumnForm[{
   img1, 
   Spacer[10], 
   ExportString[img1, {"Base64", "JPEG"}] // Short}]
```

![0wmip47gloav0](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/0wmip47gloav0.png)

### Image narration

Here is an image narration example with the two images above, again, one specified with a URL, the other with a file path:

```mathematica
LLMVisionSynthesize["Give concise descriptions of the images.", {"https://i.imgur.com/LEGfCeql.jpg", $HomeDirectory <> "/Downloads/ThreeHunters.jpg"}, "MaxTokens" -> 600]
```

```
1. The first image depicts a single raccoon perched on a tree branch, surrounded by a plethora of vibrant,
   colorful butterflies in various shades of blue, orange, and other colors, set against a lush, multicolored foliage background.
2. The second image shows three raccoons sitting together on a tree branch in a forest setting, with a warm, 
   glowing light illuminating the scene from behind. The forest is teeming with butterflies, 
   matching the one in the first image, creating a sense of continuity and shared environment between the two scenes.
```

### Description of a mind-map

Here is an application that should be more appealing to WL-developers -- getting a description of a technical diagram or flowchart. Well, in this case, it is a mind-map from [AA2]:

```mathematica
Import[URL["https://i.imgur.com/kcUcWnql.jpeg"]]
```

![1ukmn97ui4o98](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/1ukmn97ui4o98.png)

Here are get the vision model description of the mind-map above (and place the output in Markdown format):

```mathematica
mmDescr = LLMVisionSynthesize["How many branches this mind-map has? Describe each branch separately. Use relevant emoji prefixes.", "https://imgur.com/kcUcWnq.jpeg", "MaxTokens" -> 900]
```

```
This mind map has four primary branches, each diverging from a \
central node labeled "LLM functionalities." I will describe each one \
using relevant emoji prefixes:

1. 🖼️ **DALL-E** branch is in yellow and represents an access point to \
the DALL-E service, likely a reference to a Large Language Model \
(LLM) with image generation capabilities.

2. 🤖 **ChatGPT** branch in pink is associated with the ChatGPT \
service, suggesting it's a conversational LLM branch. There are two \
sub-branches:
   - **LLM prompts** indicates a focus on the prompts used to \
communicate with LLMs.
   - **Notebook-wide chats** suggests a feature or functionality for \
conducting chats across an entire notebook environment.

3. 💬 **LLM chat objects** branch in purple implies that there are \
objects specifically designed for chat interactions within LLM \
services.

4. ✍️ **LLM functions** branch in green seems to represent various \
functional aspects or capabilities of LLMs, with a sub-branch:
   - **Chatbooks** which may indicate a feature or tool related to \
managing or organizing chat conversations as books or records.
```

### Converting descriptions to diagrams

Here from the obtained description we request a (new) Mermaid-JS diagram to be generated:

```mathematica
mmdChart = LLMSynthesize[{LLMPrompt["CodeWriter"], "Make the corresponding Mermaid-JS diagram code for the following description. Give the code only, without Markdown symbols.", mmDescr}]
```

```
graph TB
    center[LLM functionalities]
    center --> dalle[DALL-E]
    center --> chat[ChatGPT]
    center --> chatobj[LLM chat objects]
    center --> functions[LLM functions]
    chat --> prompts[LLM prompts]
    chat --> notebook[Notebook-wide chats]
    functions --> chatbooks[Chatbooks]
```

Here is a diagram made with the Mermaid-JS spec obtained above using the resource function of 
["MermaidInk"](https://resources.wolframcloud.com/FunctionRepository/resources/MermaidInk/), [AAf1]:

```mathematica
ResourceFunction["MermaidInk"][mmdChart]
```

![1qni2g4n8vywf](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/1qni2g4n8vywf.png)

Below is given an instance of one of the better LLM results for making a Mermaid-JS diagram over the "vision-derived" mind-map description.

```mathematica
ResourceFunction["MermaidInk"]["graph TBA[LLM services access] --> B[DALL-E]A --> C[ChatGPT]A --> D[PaLM]A --> E[LLM chat objects]A --> F[Chatbooks]B -->|related to| G[DALL-E AI system]C -->|associated with| H[ChatGPT]D -->|related to| I[PaLM model]E -->|part of| J[chat-related objects/functionalities]F -->|implies| K[Feature or application related to chatbooks]"]
```

![0f0fuo9nexxl8](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/0f0fuo9nexxl8.png)

### Code generation from image descriptions

Here is an example of code generation based on the "vision derived" mind-map description above:

```mathematica
LLMSynthesize[{LLMPrompt["CodeWriter"], "Generate the Mathematica code of a graph that corresponds to the description:\n", mmDescr}]
```

```
Graph[{"LLM services access" -> "DALL-E","LLM services access" -> "ChatGPT",
"LLM services access" -> "PaLM",
"LLM services access" -> "LLM functionalities",
"LLM services access" -> "Chatbooks","LLM services access" -> "Notebook-wide chats",
"LLM services access" -> "Direct access of LLM services","LLM functionalities" -> "LLM prompts",
"LLM functionalities" -> "LLM functions","LLM functionalities" -> "LLM chat objects"},
VertexLabels -> "Name"]
```

```mathematica
ToExpression[%]
```

![0cmyq0lep1q7f](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/0cmyq0lep1q7f.png)

### Analyzing graphical WL results

Consider another "serious" example -- that of analyzing chess play positions. Here we get a chess position using the paclet ["Chess"](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/Chess/), [WRIp3]:

```mathematica
b = PacletSymbol["Wolfram/Chess", "Wolfram`Chess`Chessboard"]["2kr1b1r/ppp1qppp/2n2n2/3p1b2/3P1N2/2P1B3/PP1NQPPP/2KR1B1R b - - 3 11"]["Graphics"];
b2 = b /. HoldPattern[(FrameTicks -> x_)] :> (FrameTicks -> (x /. y_String -> Style[ToUpperCase@y, FontSize -> 14]))
```

![0scq7lbpp7xfs](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/0scq7lbpp7xfs.png)

Here we describe it with "AI vision":

```mathematica
LLMVisionSynthesize["Describe the position.", Image[b2], "MaxTokens" -> 1000, "Temperature" -> 0.05]
```

```
This is a chess position from a game in progress. Here's the \
description of the position by algebraic notation for each piece:

White pieces:
- King (K) on c1
- Queen (Q) on e2
- Rooks (R) on h1 and a1
- Bishops (B) on e3 and f1
- Knights (N) on g4 and e2
- Pawns (P) on a2, b2, c4, d4, f2, g2, and h2

Black pieces:
- King (K) on e8
- Queen (Q) on e7
- Rooks (R) on h8 and a8
- Bishops (B) on f5 and g7
- Knights (N) on c6 and f6
- Pawns (P) on a7, b7, c7, d7, f7, g7, and h7

It's Black's turn to move. The position suggests an ongoing middle \
game with both sides having developed most of their pieces. White has \
castled queenside, while Black has not yet castled. The white knight \
on g4 is putting pressure on the black knight on f6 and the pawn on \
h7. The black bishop on f5 is active and could become a strong piece \
depending on the continuation of the game.
```

**Remark:** In our few experiments with these kind of image narrations, a fair amount of the individual pieces 
are described to be at wrong chessboard locations.

**Remark:** In order to make the AI vision more successful, we increased the size of the chessboard frame tick labels, 
and turned the “a÷h” ticks uppercase (into “A÷H” ticks.) 
It is interesting  to compare the vision results over chess positions with and without that transformation.

## LLM Functions

Let us show more programmatic utilization of the vision capabilities.

Here is the workflow we consider:

1. Ingest an image file and encode it into a Base64 string

2. Make an LLM configuration with that image string (and a suitable model)

3. Synthesize a response to a basic request (like, image description)

    - Using LLMSynthesize

4. Make an LLM function for asking different questions over image

    - Using LLMFunction

5. Ask questions and verify results

    - ⚠️ *Answers to "hard" numerical questions are often wrong.*

    - It might be useful to get formatted outputs

**Remark:** The function `LLMVisionSynthesize` combines `LLMSynthesize` and step 2. 
The function `LLMVisionFunction` combines `LLMFunction` and step 2.

### Image ingestion and encoding

Here we ingest an image and display it:

```mathematica
imgBarChart = Import[$HomeDirectory <> "/Downloads/Cyber-Week-Spending-Set-to-Hit-New-Highs-in-2023-small.jpeg"]
```

![0iyello2xfyfo](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/0iyello2xfyfo.png)

**Remark:** The image was downloaded from the post ["Cyber Week Spending Set to Hit New Highs in 2023"](https://www.statista.com/chart/7045/thanksgiving-weekend-e-commerce-sales/) .

### Configuration and synthesis

Here we synthesize a response of an image description request:

```mathematica
LLMVisionSynthesize["Describe the image.", imgBarChart, "MaxTokens" -> 600]
```

```
The image shows a bar chart infographic titled "Cyber Week Spending \
Set to Hit New Highs in 2023" with a subtitle "Estimated online \
spending on Thanksgiving weekend in the United States." There are \
bars for five years (2019, 2020, 2021, 2022, and 2023) across three \
significant shopping days: Thanksgiving Day, Black Friday, and Cyber \
Monday.

The bars represent the spending amounts, with different colors for \
each year. The spending for 2019 is shown in navy blue, 2020 in a \
lighter blue, 2021 in yellow, 2022 in darker yellow, and 2023 in dark \
yellow, with a pattern that clearly indicates the 2023 data is a \
forecast.

From the graph, one can observe an increasing trend in estimated \
online spending, with the forecast for 2023 being the highest across \
all three days. The graph also has an icon that represents online \
shopping, consisting of a computer monitor with a shopping tag.

At the bottom of the infographic, there is a note that says the \
data's source is Adobe Analytics. The image also contains the \
Statista logo, which indicates that this graphic might have been \
created or distributed by Statista, a company that specializes in \
market and consumer data. Additionally, there are Creative Commons \
(CC) icons, signifying the sharing and use permissions of the graphic.

It's important to note that without specific numbers, I cannot \
provide actual figures, but the visual trend is clear -- \
there is substantial year-over-year growth in online spending during \
these key shopping dates, culminating in a forecasted peak for 2023.
```

### Repeated questioning

Here we define an LLM function that allows multiple question request invocations over the image:

```mathematica
fst = LLMVisionFunction["For the given image answer the question: ``. Be as concise as possible in your answers.", imgBarChart, "MaxTokens" -> 300]
```

![0nmz56wwuboz3](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/0nmz56wwuboz3.png)

```mathematica
fst["How many years are presented in that image?"]
```

```
"Five years are presented in the image."
```

```mathematica
fst["Which year has the highest value? What is that value?"]
```

```
"2023 has the highest value, which is approximately $11B on Cyber Monday."
```

**Remark:** Numerical value readings over technical plots or charts seem to be often wrong. 
OpenAI's vision model warns about this in the responses often enough.

### Formatted output

Here we make a function with a specially formatted output that can be more easily integrated in (larger) workflows:

```mathematica
fjs = LLMVisionFunction["How many `1` per `2`? " <> LLMPrompt["NothingElse"]["JSON"], imgBarChart, "MaxTokens" -> 300, "Temperature" -> 0.1]
```

![032vcq74auyv9](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/032vcq74auyv9.png)

Here we invoke that function (in order to get the money per year "seen" by OpenAI's vision):

```mathematica
res = fjs["money", "shopping day"]
```

````
```json
{
  "Thanksgiving Day": {
    "2019": "$4B",
    "2020": "$5B",
    "2021": "$6B",
    "2022": "$7B",
    "2023": "$8B"
  },
  "Black Friday": {
    "2019": "$7B",
    "2020": "$9B",
    "2021": "$9B",
    "2022": "$10B",
    "2023": "$11B"
  },
  "Cyber Monday": {
    "2019": "$9B",
    "2020": "$11B",
    "2021": "$11B",
    "2022": "$12B",
    "2023": "$13B"
  }
}
```
````

**Remark:**  The above result should be structured as shopping-day:year:value. 
But occasionally it might be structured as year::shopping-day::value. 
In the latter case just re-run LLM invocation.

Here we parse the obtained JSON into WL association structure:

```mathematica
aMoney = ImportString[StringReplace[res, {"```json" -> "", "```" -> ""}], "RawJSON"]
```

```
<|"Thanksgiving Day" -> <|"2019" -> "$4B", "2020" -> "$5B", 
   "2021" -> "$6B", "2022" -> "$7B", "2023" -> "$8B"|>, 
 "Black Friday" -> <|"2019" -> "$7B", "2020" -> "$9B", 
   "2021" -> "$9B", "2022" -> "$10B", "2023" -> "$11B"|>, 
 "Cyber Monday" -> <|"2019" -> "$9B", "2020" -> "$11B", 
   "2021" -> "$11B", "2022" -> "$12B", "2023" -> "$13B"|>|>
```

**Remark:** Currently `LLMVisionFunction` does not have an interpreter (or "form") parameter as `LLMFunction` does. 
This can be seen as one of the reasons to include `LLMVisionFunction` in the "LLMFunctions" framework.

Here we convert the money strings into money quantities:

```mathematica
AbsoluteTiming[
  aMoney2 = Map[SemanticInterpretation, aMoney, {-1}] 
 ]
```

![08ijuwuchj31q](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/08ijuwuchj31q.png)

Here is the corresponding bar chart and the original bar chart (for comparison):

```mathematica
ColumnForm[{BarChart[aMoney2,
  ChartLabels -> {Placed[(Style[#1, White, Bold, FontSize -> 16] &) /@ Keys[aMoney2], Center], Keys[aMoney2[[1]]]},
  PlotTheme -> "Scientific", GridLines -> Automatic,
  ImageSize -> 600], Spacer[10],
  Show[ImageTake[imgBarChart, {200, -100}], ImageSize -> 650]}]
```

![1lpfhko7c2g6e](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/1lpfhko7c2g6e.png)

**Remark:** The comparison shows "pretty good vision" by OpenAI! But, again, small (or maybe significant) discrepancies are observed.

## Dedicated notebook cells

In the context of the "well-established" notebook solutions OpenAIMode, [AAp2], or [C](https://raku.land/zef:antononcube/Jupyter::Chatbook)hatbook, [WRIp2], we can contemplate extensions to integrate OpenAI's vision service.

The main challenges here include determining how users will specify images in the notebook, such as through URLs, file names, or Base64 strings, each with unique considerations. Additionally, we have to explore how best to enable users to input prompts or requests for image processing by the AI/LLM service.

This integration, while valuable, it is not *my* immediate focus as there are programmatic ways to access OpenAI's vision service already. (See the previous sections.)

## Combinations (fairytale generation)

Consider the following computational workflow for making fairytales:

1. Draw or LLM-generate a few images that characterize parts of a story.

1. Narrate the images using the LLM "vision" functionality.

1. Use an LLM to generate a story over the narrations.

**Remark:** Multi-modal LLM / AI systems already combine steps 2 and 3.

**Remark:** The workflow above (after it is programmed) can be executed multiple times until satisfactory results are obtained.

Here are image generations using DALL-E for four different requests with the same illustrator name in them:

```mathematica
storyImages = 
   Map[
    ImageSynthesize["Painting in the style of John Bauer of " <> #] &,
    {"a girl gets a basket with wine and food for her grandma.", 
     "a big bear meets a girl carrying a basket in the forest.", 
     "a girl that gives food from a basket to a big bear.", 
     "a big bear builds a new house for girl's grandma."} 
   ];
storyImages // Length

(*4*)
```

Here we display the images:

```mathematica
storyImages
```

![13qqfe3pzqfn9](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MarkdownDocuments/Diagrams/AI-vision-via-WL/13qqfe3pzqfn9.png)

Here we get the image narrations (via the OpenAI's "vision service"):

```mathematica
storyImagesDescriptions = LLMVisionSynthesize["Concisely describe the images.", storyImages, "MaxTokens" -> 600]
```

```
1. A painting of a woman in a traditional outfit reaching into a
    basket filled with vegetables and bread beside a bottle.
2. An illustration of a person in a cloak holding a bucket and
    standing next to a large bear in a forest.
3. An artwork depicting a person sitting naked by a birch tree,
    sharing a cake with a small bear.
4. A picture of a person in a folk costume sitting next to a bear
    with a ladder leaning against a house.
```

Here we extract the descriptions into a list:

```mathematica
descr = StringSplit[storyImagesDescriptions, "\n"];
```

Here we generate the story from the descriptions above (using OpenAI's ChatGPT):

```mathematica
 LLMSynthesize[{"Write a story that fits the following four descriptions:", Sequence @@ descr}, LLMEvaluator -> LLMConfiguration["MaxTokens" -> 1200]]
```

```
In a small village nestled deep within a lush forest, lived a woman \
named Anya. She was gentle and kind-hearted, known for her artistic \
talent and love for nature. Anya had a keen eye for capturing the \
beauty of the world around her through her paintings. Each stroke of \
her brush seemed to hold a piece of her soul, and her art touched the \
hearts of all who laid their eyes upon it.

One sunny day, on the outskirts of the village, Anya set up her easel \
amidst a lively farmers' market. In front of her, she placed a large \
canvas, ready to bring her latest vision to life. With her palette \
filled with vibrant colors, she began painting a woman dressed in a \
traditional outfit, delicately reaching into a woven basket filled to \
the brim with fresh vegetables and warm bread. Beside the basket lay \
an empty bottle, hinting at a joyous feast anticipated for the day.

As Anya skillfully brought her painting to life, a cloak-wrapped \
figure caught her attention. Intrigued, she turned her easel slightly \
to capture the essence of this mysterious wanderer standing beside a \
mighty bear deep within the heart of the forest. In her illustration, \
she depicted the cloaked person, holding a bucket, their gaze met by \
the curious eyes of the regal woodland creature. The bond between \
them was palpable, a silent understanding as they stood together, \
guardians of the ancient woods.

Meanwhile, in a clearing not too far away, Anya discovered a scene \
that touched her deeply. She stumbled upon a person sitting naked \
beneath the shade of a majestic birch tree, a cake placed lovingly \
between them and a small bear. The artwork she created was a tender \
portrayal of the intimate connection shared by the two, a testament \
to the innate kindness that existed between species. Together, they \
enjoyed the sweet treat, their hearts entwined in empathy, neither \
fearing the vulnerability of their exposed selves.

Driven by her artistry, Anya's imagination continued to explore the \
fascinating relationship between humans and bears in her village. In \
her final artwork, she turned her focus to a person in a folk \
costume, sitting comfortably beside a towering bear. A ladder leaned \
against a charming wooden house in the background, illustrating the \
close bond shared between the village folks and their wild \
companions. Together, they stood tall, their spirits entwined in a \
balance of mutual respect and harmony.

As Anya showcased her artwork to the villagers, they were captivated \
by the depth of emotion expressed through her brushstrokes. Her \
paintings served as a reminder that love and understanding knew no \
boundaries, whether lived within the confines of villages or amidst \
the enchanting wilderness.

Anya became a celebrated artist, known far and wide for her ability \
to weave tales of compassion and unity through her exquisite \
paintings. Her work inspired generations to see the world through the \
lens of empathy, teaching them that even in unconventional \
connections between humans and animals, beauty could be found.

And so, her legacy lived on, her art continuing to touch the hearts \
of those who recognized the profound messages hidden within her \
strokes of color. For in every stroke, Anya immortalized the timeless \
bond between humanity and the natural world, forever reminding us of \
the kinship we share with the creatures that roam our earth.
```

## Conclusions and leftover comments

- The new OpenAI vision model, "gpt-4-vision-preview", as all LLMs produces too much words, and it has to be reined in and restricted.

- The functions LLMVisionSynthesize and LLMVisionFunction have to be part of the "LLMFunctions" framework.

    - For example, "LLMVision*" functions do not have an interpreter (or "form") argument.

- The package "LLMVision" is meant to be simple and direct, not covering all angles.

    - Very likely similar motivation was behind the creation of the post/notebook ["Direct API access to new features of GPT-4 (including vision, DALL-E, and TTS)​​"](https://community.wolfram.com/groups/-/m/t/3062403), [MT1].

    - The package "LLMVision" uses the simple, OpenAI access providing package "OpenAIRequest", [AAp3], which is based on code from "OpenAILink", [CWp1].

- It would be nice a dedicated notebook cell interface and workflow(s) for interacting with "AI vision" services to be designed and implemented.

    - The main challenge is the input of images.

- Generating code from hand-written diagrams might be really effective demo using WL.

- It would be interesting to apply the "AI vision" functionalities over displays from, say, chess or play-cards paclets. 

## References

### Articles

[AA1] Anton Antonov, ["Workflows with LLM functions (in WL)"](https://community.wolfram.com/groups/-/m/t/2983602),​ August 4, (2023), Wolfram Community, STAFF PICKS.

[AA2] Anton Antonov, ["Raku, Python, and Wolfram Language over LLM functionalities"](https://community.wolfram.com/groups/-/m/t/3053519), (2023), [Wolfram Community](https://community.wolfram.com).

[AA3] Anton Antonov, ["AI vision via Raku"](https://community.wolfram.com/groups/-/m/t/3071989), (2023), [Wolfram Community](https://community.wolfram.com).

[MT1] Marco Thiel, ["Direct API access to new features of GPT-4 (including vision, DALL-E, and TTS)​​"](https://community.wolfram.com/groups/-/m/t/3062403), November 8, (2023), Wolfram Community, STAFF PICKS.

[OAIb1] OpenAI team, ["New models and developer products announced at DevDay"](https://openai.com/blog/new-models-and-developer-products-announced-at-devday) , (2023), [OpenAI/blog](https://openai.com/blog) .

### Functions, packages, and paclets

[AAf1] Anton Antonov, [MermaidInk](https://resources.wolframcloud.com/FunctionRepository/resources/MermaidInk/), WL function, (2023), [Wolfram Function Repository](https://resources.wolframcloud.com/FunctionRepository).

[AAp1] Anton Antonov, [LLMVision.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/LLMVision.m), Mathematica package, (2023), [GitHub/antononcube](https://github.com/antononcube) .

[AAp2] Anton Antonov, [OpenAIMode](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/OpenAIMode/), WL paclet, (2023), [Wolfram Language Paclet Repository](https://resources.wolframcloud.com/PacletRepository/).

[AAp3] Anton Antonov, [OpenAIRequest.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/OpenAIRequest.m), Mathematica package, (2023), [GitHub/antononcube](https://github.com/antononcube) .

[CWp1] Christopher Wolfram, [OpenAILink](https://resources.wolframcloud.com/PacletRepository/resources/ChristopherWolfram/OpenAILink/), WL paclet, (2023), [Wolfram Language Paclet Repository](https://resources.wolframcloud.com/PacletRepository/).

[WRIp1] Wolfram Research, Inc., [LLMFunctions](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/LLMFunctions/), WL paclet, (2023), [Wolfram Language Paclet Repository](https://resources.wolframcloud.com/PacletRepository/).

[WRIp2] Wolfram Research, Inc., [Chatbook](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/Chatbook/), WL paclet, (2023), [Wolfram Language Paclet Repository](https://resources.wolframcloud.com/PacletRepository/).

[WRIp3] Wolfram Research, Inc., [Chess](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/Chess/), WL paclet, (2023), [Wolfram Language Paclet Repository](https://resources.wolframcloud.com/PacletRepository/).

### Videos

[AAv1] Anton Antonov, ["OpenAIMode demo (Mathematica)"](https://www.youtube.com/watch?v=htUIOqcS9uA), (2023), [YouTube/@AAA4Prediction](https://www.youtube.com/@AAA4prediction) .