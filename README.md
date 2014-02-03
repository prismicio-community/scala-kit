## Scala development kit for prismic.io

### Getting started

#### Install the kit for your project with Maven

You can find this library in our own Maven repository (hosted on Github).

If you are using __sbt__, first add the reposiotry resolver:

```
resolvers += "Prismic.io kits" at "https://github.com/prismicio/repository/raw/master/maven/"
```

Then add, the library dependency:

```
"io.prismic" %% "scala-kit" % "1.0-M5"
```

*(you may need to adapt the version number)*


#### Get started with prismic.io

You can find out [how to get started with prismic.io](https://developers.prismic.io/documentation/UjBaQsuvzdIHvE4D/getting-started) on our [prismic.io developer's portal](https://developers.prismic.io/).

#### Get started using the kit

Also on our [prismic.io developer's portal](https://developers.prismic.io/), on top of our full documentation, you will:
 * get a thorough introduction of [how to use prismic.io kits](https://developers.prismic.io/documentation/UjBe8bGIJ3EKtgBZ/api-documentation#kits-and-helpers), including this one.
 * see [what else is available for Scala](https://developers.prismic.io/technologies/UjBh4cuvzeMJvE4k/scala): starter projects, examples, ...


#### Kit's detailed documentation

You can find [the documentation of the Scala kit as a Scaladoc](http://prismicio.github.io/scala-kit/).

### Changelog

Need to see what changed, or to upgrade your kit? We keep our changelog on [this repository's "Releases" tab](https://github.com/prismicio/ruby-kit/releases).

### Contribute to the kit

Contribution is open to all developer levels, read our "[Contribute to the official kits](https://developers.prismic.io/documentation/UszOeAEAANUlwFpp/contribute-to-the-official-kits)" documentation to learn more.

#### Install the kit locally

Fork and clone the repository, and run `sbt compile` from the newly-created directory ([install sbt](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html) first if you haven't)

#### Test

Please write tests for any bugfix or new feature.

If you find existing code that is not optimally tested and wish to make it better, we really appreciate it; but you should document it on its own branch and its own pull request.

#### Documentation

Please document any bugfix or new feature.

If you find existing code that is not optimally documented and wish to make it better, we really appreciate it; but you should document it on its own branch and its own pull request.

### Licence

This software is licensed under the Apache 2 license, quoted below.

Copyright 2013 Zengularity (http://www.zengularity.com).

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this project except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
