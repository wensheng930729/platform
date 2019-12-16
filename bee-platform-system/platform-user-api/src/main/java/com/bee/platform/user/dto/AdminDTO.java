package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName AdminDTO
 * @Description 管理员信息
 * @author jie.chen
 * @Date 2019/3/19$ 14:07$
 * @version 1.0.0
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("管理员信息")
public class AdminDTO implements Serializable {

    private static final long serialVersionUID = -3658974307902889560L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("角色")
    private String type;

    @ApiModelProperty("部门")
    private List<DepartmentDTO> department;

    @ApiModelProperty("手机号")
    private String phone;

    @ApiModelProperty("姓名")
    private String nickname;

    @ApiModelProperty("职位")
    private String post;

}
