;;; fb2-parser-test.el --- Tests for fb2 parser
;;; Commentary:
;;; Code:
(load (expand-file-name "fb2-reader.el" default-directory))
(describe "Dummy test for fb2 parser"
	  :var ((fb2-content "<?xml version=\"1.0\" encoding=\"windows-1251\"?>
<FictionBook xmlns=\"http://www.gribuser.ru/xml/fictionbook/2.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">
  <description xmlns:l=\"http://www.w3.org/1999/xlink\">
    <title-info>
      <genre>lorem</genre>
      <author>
        <first-name>Lorem</first-name>
        <last-name>Ipsum</last-name>
      </author>
      <book-title>Some strange title</book-title>
      <annotation>
        <p>Very interesting description</p>
      </annotation>
      <keywords>some keywoid</keywords>
      <lang>lang</lang>
      <src-lang>lang</src-lang>
      <translator>
        <first-name>Lorem</first-name>
        <middle-name>Ip</middle-name>
        <last-name>Sum</last-name>
      </translator>
      <sequence name=\"lorem\" number=\"3\"/>
    </title-info>
    <src-title-info>
      <genre>lorem</genre>
      <author>
        <first-name>Lorem</first-name>
        <last-name>Ipsum</last-name>
        <home-page>lorem.com</home-page>
        <id>222222</id>
      </author>
      <book-title>Lorem Ipsum</book-title>
      <annotation>
        <p>Still interesting</p>
      </annotation>
      <lang>lorem</lang>
      <sequence name=\"Lorem\" number=\"3\"/>
    </src-title-info>
    <document-info>
      <version/>
    </document-info>
    <publish-info>
      <book-name>Lorem Ipsum Book</book-name>
      <publisher>LI</publisher>
      <year>2222</year>
      <isbn>ISBN 0000000</isbn>
      <sequence name=\"Lorem Ipsum\"/>
    </publish-info>
    <custom-info info-type=\"lorem-id\">12121212</custom-info>
  </description>
  <body>
    <section>
      <title>
        <p>Lorem.</p>
        <p>Ipsum,</p>
        <p>Dolores</p>
      </title>
      <image xlink:href=\"#vvv.jpg\"/>
      <section>
        <epigraph>
          <p>
            <emphasis>Mauris ac felis vel velit tristique imperdiet.</emphasis>
          </p>
          <p>
            <emphasis>Nullam tristique diam non turpis.  </emphasis>
          </p>
          <text-author>Ipsum</text-author>
        </epigraph>
        <empty-line/>
      </section>
      <section>
        <title>
          <p>Ipsum</p>
        </title>
        <section>
          <epigraph>
            <p>
              <emphasis>Pellentesque condimentum, magna ut suscipit hendrerit, ipsum augue ornare nulla, non luctus diam neque sit amet urna.  </emphasis>
            </p>
            <text-author>Lorem Ipsum</text-author>
          </epigraph>
          <empty-line/>
        </section>
        <section>
          <title>
            <p>Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit.  </p>
          </title>
          <p>Aliquam erat volutpat.  Nunc eleifend leo vitae magna.  In id erat non orci commodo lobortis.  Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus.  Sed diam.  Praesent fermentum tempor tellus.  Nullam tempus.  Mauris ac felis vel velit tristique imperdiet.  Donec at pede.  Etiam vel neque nec dui dignissim bibendum.  Vivamus id enim.  Phasellus neque orci, porta a, aliquet quis, semper a, massa.  Phasellus purus.  Pellentesque tristique imperdiet tortor.  Nam euismod tellus id erat.</p>
	</section>
	</section>
	</section>
      </body>
    </FictionBook>"))
	  (it "just tests if parser works without errors"
	      (with-temp-buffer
		(insert fb2-content)
		(fb2-reader-render (xml-parse-region (point-min) (point-max)))))
	  )
;;; fb2-parser-test.el ends here
