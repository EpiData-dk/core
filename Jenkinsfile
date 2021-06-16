@Library('epidata.pipeline') _

env.FPC_VERSION='3.2.0'
env.CORE_VERSION='4.0.0'

node {
    stage("Source Checkout") {
        checkout scm
    }

    stage("Linux-64") {
        epidocker.run "mvn compile -Plinux64,core"
        epidocker.run "mvn compile -Plinux64,visuals"
    }

    stage("Win-64") {
        epidocker.run "mvn compile -Pwin64,core"
        epidocker.run "mvn compile -Pwin64,visuals"
    }

    stage("Mac-64") {
        epidocker.run "mvn compile -Pdarwin64,core"
        epidocker.run "mvn compile -Pdarwin64,visuals"
    }

    stage("Package & Deploy") {
        sh "mvn versions:set -DnewVersion=${CORE_VERSION}.${BUILD_ID} -f deploy.pom.xml"
        sh "mvn deploy -f deploy.pom.xml"
        sh "mvn clean"
    }
}