import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "io.stryker-mutator"
version = "1.0"

plugins {
    kotlin("jvm") version "1.4.20"
    kotlin("plugin.serialization") version "1.4.20"
    `java-library`
    `maven-publish`
    signing
    jacoco
//    id("io.gitlab.arturbosch.detekt") version "1.17.1"
    id("org.sonarqube") version "3.3"
    id("com.diffplug.gradle.spotless") version "4.5.1"
}

repositories {
    mavenCentral()
    mavenLocal()
}

dependencies {
    implementation("io.stryker-mutator:stryker4jvm-core:1.0")
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8:1.4.20")
    testImplementation(kotlin("test-junit"))
    testImplementation("io.mockk:mockk:1.10.6")
    implementation("org.jetbrains.kotlin:kotlin-compiler-embeddable:1.4.20")
//    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.0.1")
//    implementation("io.stryker-mutator:mutation-testing-elements:1.7.0")
//    implementation("io.stryker-mutator:mutation-testing-metrics-circe_2.13:1.7.0")
    implementation(gradleApi())
}

tasks.test {
    useJUnit()
    finalizedBy(tasks.jacocoTestReport) // report is always generated after tests run
}

tasks.jacocoTestReport {
    dependsOn(tasks.test) // tests are required to run before generating the report
    reports {
        xml.isEnabled = true
    }
}

tasks.register<Test>("stryker") {
    outputs.upToDateWhen { false }
    failFast = true
    useJUnit()
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "11"
}

tasks.jar {
    manifest {
        attributes(
            mapOf(
                "Implementation-Title" to project.name,
                "Implementation-Version" to project.version
            )
        )
    }
}

val sourceJar by tasks.creating(Jar::class) {
    from(sourceSets.getByName("main").allSource)
    archiveClassifier.set("sources")
}

val javadocJar by tasks.creating(Jar::class) {
    from(tasks.getByName("javadoc"))
    archiveClassifier.set("javadoc")
}

tasks.withType(Javadoc::class) {
    (options as StandardJavadocDocletOptions).addStringOption("Xdoclint:none", "-quiet")
}

publishing {
    publications {
        create<MavenPublication>("stryker4jvm-mutator-kotlin") {
            pom {
                name.set("Stryker4jvm mutator kotlin")
//                description.set("Stryker4k, the mutation testing framework for Kotlin. Core library")
//                url.set("https://github.com/stryker-mutator/stryker4k")
//                inceptionYear.set("2021")
//                licenses {
//                    license {
//                        name.set("The Apache License, Version 2.0")
//                        url.set("http://www.apache.org/licenses/LICENSE-2.0.txt")
//                    }
//                }
//                developers {
//                    developer {
//                        id.set("Nickmanbear")
//                        name.set("Kees Vaes")
//                    }
//                }
//                scm {
//                    connection.set("scm:https://github.com/stryker-mutator/stryker4k.git")
//                    developerConnection.set("scm:git@github.com:stryker-mutator/stryker4k.git")
//                    url.set("https://github.com/stryker-mutator/stryker4k")
//                }
            }
            groupId = "io.stryker-mutator"
            artifactId = "stryker4jvm-mutator-kotlin"
            version = project.version.toString()

            artifact(javadocJar)
            artifact(sourceJar)

            from(components["java"])
        }
    }
}
// todo: enable this when the module is finished and should be published
// signing {
//    useGpgCmd()
//
//    sign(publishing.publications["stryker4k-core"])
// }

tasks.withType(Javadoc::class) {
    (options as StandardJavadocDocletOptions).addStringOption("Xdoclint:none", "-quiet")
}

sonarqube {
    properties {
        property("sonar.kotlin.detekt.reportPaths", "build/reports/detekt/detekt.xml")
        property("sonar.jacoco.reportPaths", "build/reports/jacoco")
    }
}

// https://github.com/diffplug/spotless/tree/main/plugin-gradle#kotlin
// spotless { // if you are using build.gradle.kts, instead of 'spotless {' use:
configure<com.diffplug.gradle.spotless.SpotlessExtension> {
    kotlin {
        // by default the target is every '.kt' and '.kts` file in the java sourcesets
        ktfmt("0.24") // has its own section below
        // ktlint()   // has its own section below
        // diktat()   // has its own section below
        // prettier() // has its own section below
        // licenseHeader '/* (C)$YEAR */' // or licenseHeaderFile
    }

    kotlinGradle {
        target("*.gradle.kts") // default target for kotlinGradle
        ktlint() // or ktfmt() or prettier()
    }
}
