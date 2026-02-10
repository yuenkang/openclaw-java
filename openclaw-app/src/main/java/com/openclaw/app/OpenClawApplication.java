package com.openclaw.app;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableScheduling;

/**
 * OpenClaw Java application entry point.
 */
@SpringBootApplication
@EnableScheduling
@ComponentScan(basePackages = "com.openclaw")
public class OpenClawApplication {

    public static void main(String[] args) {
        SpringApplication.run(OpenClawApplication.class, args);
    }
}
