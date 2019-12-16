package com.bee.platform.user.dto;

import com.bee.platform.common.entity.DepartmentInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @description: 企业用户信息
 * @author: junyang.li
 * @create: 2019-03-21 09:52
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业用户信息返回对象")
public class EnterprisesUsersDTO implements Serializable {

    private static final long serialVersionUID = -2825214752795500872L;

    @ApiModelProperty("企业id")
    private Integer enterpriseId;

    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("用户账号")
    private String username;

    @ApiModelProperty("职位")
    private String post;

    @ApiModelProperty("是否激活")
    private Integer isInvite;

    @ApiModelProperty("是否邀请")
    private Integer isActive;

    @ApiModelProperty("姓名")
    private String nickname;

    @ApiModelProperty("姓名拼音")
    private String nicknamePinyin;

    @ApiModelProperty("应用id")
    private String appIds;

    @ApiModelProperty("部门信息")
    private List<DepartmentDTO> departments;

    @ApiModelProperty("角色id")
    private Integer roleId;
    private DepartmentInfo department;
}
