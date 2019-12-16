package com.bee.platform.user.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * @description:
 * @author: junyang.li
 * @create: 2018-11-30 08:59
 **/
@ApiModel("企业查询传入参数")
public class EnterpriseDTO implements Serializable {

    private static final long serialVersionUID = -4019016898528943826L;

    @ApiModelProperty("账号")
    private String username;

    @ApiModelProperty("密码")
    private String password;

    @ApiModelProperty("企业ID")
    @NotNull(message = "企业ID不能为空")
    private List<Integer> orgId;

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public List<Integer> getOrgId() {
        return orgId;
    }

    public void setOrgId(List<Integer> orgId) {
        this.orgId = orgId;
    }
}
