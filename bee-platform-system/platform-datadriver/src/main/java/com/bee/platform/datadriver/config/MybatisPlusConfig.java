package com.bee.platform.datadriver.config;

import com.alibaba.druid.spring.boot.autoconfigure.DruidDataSourceBuilder;
import com.baomidou.mybatisplus.plugins.PaginationInterceptor;
import com.baomidou.mybatisplus.plugins.PerformanceInterceptor;
import com.bee.platform.common.constants.MapperScanPackagesConstant;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;

@EnableTransactionManagement
@Configuration
@MapperScan(basePackages = MapperScanPackagesConstant.DATADRIVER_MAPPER_BASEPACKAGE)
public class MybatisPlusConfig {

    /**
     * mybatis-plus 的性能优化
     */
    @Bean
    @Profile({"dev","qa"})// 设置 dev test 环境开启
    public PerformanceInterceptor performanceInterceptor() {
        PerformanceInterceptor performanceInterceptor = new PerformanceInterceptor();
        /*SQL 执行性能分析，开发环境使用，线上不推荐。 maxTime 指的是 sql 最大执行时长 */
        performanceInterceptor.setMaxTime(1000);
        /*SQL是否格式化 默认false*/
        performanceInterceptor.setFormat(true);
        return performanceInterceptor;
    }

    /**
     * @Description : mybatis-plus分页插件
     */
    @Bean
    public PaginationInterceptor paginationInterceptor() {
        return new PaginationInterceptor();
    }

    /**
     * @Description : druid注入
     */
    @Bean
    @ConfigurationProperties(prefix="spring.datasource" )
    public DataSource dataSource() {
        return DruidDataSourceBuilder
                .create()
                .build();
    }
	
}
