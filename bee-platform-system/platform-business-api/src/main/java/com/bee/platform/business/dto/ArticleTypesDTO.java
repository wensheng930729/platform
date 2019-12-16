package com.bee.platform.business.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 公共类型返回数据
 * @author: jie.zhang123
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("文章返回数据")
public class ArticleTypesDTO implements Serializable{

    private static final long serialVersionUID = -5169176258151088610L;

    @ApiModelProperty("公共类型id")
    private Integer id;
    
    @ApiModelProperty("类型名")
    private String name;

    @ApiModelProperty("文章数")
    private Integer count;

}
