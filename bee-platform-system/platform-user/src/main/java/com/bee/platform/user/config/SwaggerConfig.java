package com.bee.platform.user.config;

import java.util.ArrayList;
import java.util.List;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import io.swagger.annotations.ApiOperation;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.ParameterBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.schema.ModelRef;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.service.Parameter;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;


/**
 * @ClassName SwaggerConfig
 * @Description 访问接口平台的地址http://localhost:8075/swagger-ui.html
 * @author zhigang.zhou
 * @Date 2018年11月27日 下午1:16:35
 * @version 1.0.0
 */
@Configuration
@EnableSwagger2
public class SwaggerConfig {

    @Bean
    public Docket api() {
        ParameterBuilder ticketPar1 = new ParameterBuilder();
        ticketPar1.name("sysToken").description("认证凭证")
                .modelRef(new ModelRef("string")).parameterType("header")
                .required(false)
                .defaultValue("")
                .build();
        List<Parameter> pars = new ArrayList<Parameter>();
        pars.add(ticketPar1.build());
        return new Docket(DocumentationType.SWAGGER_2)
                .apiInfo(apiInfo())
                .select()
                //这里采用包含注解的方式来确定要显示的接口
                .apis(RequestHandlerSelectors.withMethodAnnotation(ApiOperation.class))
                .paths(PathSelectors.any())
                .build()
                .globalOperationParameters(pars);

    }

    private ApiInfo apiInfo() {
        return new ApiInfoBuilder()
                .title("金蜜平台项目restful api")
                .description("platform-business api接口文档")
                //服务条款网址
                .termsOfServiceUrl("http://localhost/")
                .version("1.0.0")
                .contact(new Contact("周志钢", "", "814939649@qq.com"))
                .build();
    }
}