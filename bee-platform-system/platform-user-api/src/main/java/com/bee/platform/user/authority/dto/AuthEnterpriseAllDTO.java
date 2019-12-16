package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liliang
 * @version 1.0.0
 * @ClassName AuthRoleDTO
 * @Description 查询所有企业下拉列表的DTO
 * @Date 2019/5/27$
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("查询所有企业下拉列表的DTO")
public class AuthEnterpriseAllDTO implements Serializable {

    private static final long serialVersionUID = -6345474790320680856L;

    @ApiModelProperty("企业id")
    private Integer id;

    @ApiModelProperty("公司全称")
    private String name;
}
