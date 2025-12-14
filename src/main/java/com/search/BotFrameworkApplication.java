package com.search;

import com.search.robots.config.EnableBot;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;

@EnableBot
@SpringBootApplication
public class BotFrameworkApplication extends SpringBootServletInitializer {

	@Override
	protected SpringApplicationBuilder configure(SpringApplicationBuilder application) {
		return application.sources(BotFrameworkApplication.class);
	}

	public static void main(String[] args) {
		SpringApplication.run(BotFrameworkApplication.class, args);
	}
}
