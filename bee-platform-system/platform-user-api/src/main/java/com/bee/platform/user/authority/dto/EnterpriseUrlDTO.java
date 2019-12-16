package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName EnterpriseUrlDTO
 * @Description 功能描述
 * @Date 2019/5/27 16:06
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业下的接口url")
public class EnterpriseUrlDTO implements Serializable{
    private static final long serialVersionUID = -3792639954046571839L;

    @ApiModelProperty("企业id")
    private Integer id;

    @ApiModelProperty("url")
    private String url;
    
    @ApiModelProperty("请求类型")
    private String type;


}
