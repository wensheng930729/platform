package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author liliang
 * @since 2019-04-28
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "用户与企业关联dto")
public class UserEnterpriseDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司名称")
    private String enterprise;

    @ApiModelProperty("是否启用")
    private Integer isActive;

    @ApiModelProperty("是否管理员")
    private Integer isManager;



}
