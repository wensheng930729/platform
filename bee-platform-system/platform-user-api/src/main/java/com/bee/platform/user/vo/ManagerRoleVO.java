package com.bee.platform.user.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @description: 创建角色组的参数
 * @author: junyang.li
 * @create: 2019-04-30 11:47
 **/
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
@ApiModel("创建角色组的参数")
public class ManagerRoleVO implements Serializable {

    private static final long serialVersionUID = 8571522856375841246L;

    @ApiModelProperty(value = "权限组id")
    private Integer roleId;

    @ApiModelProperty(value = "权限组名称",required = true)
    @NotEmpty(message = "权限组名称不能为空")
    @Length(max = 15,message = "权限组名称限制15个字符")
    private String roleName;

    @ApiModelProperty(value = "基础角色id",required = true)
    @NotEmpty(message = "基础角色id不能为空")
    private List<Integer> roleIds;
}
