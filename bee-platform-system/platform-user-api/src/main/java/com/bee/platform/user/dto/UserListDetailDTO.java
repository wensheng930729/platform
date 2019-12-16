package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

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
@ApiModel(value = "用户管理列表详情dto")
public class UserListDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("用户姓名")
    private String nickname;

    @ApiModelProperty("头像")
    private String head;

    @ApiModelProperty("手机号")
    private String phone;

    @ApiModelProperty("固话")
    private String fixtel;

    @ApiModelProperty("邮箱")
    private String email;

    @ApiModelProperty("最后修改时间")
    private Date updateAt;

    @ApiModelProperty("省级地区id")
    private String provinceId;

    @ApiModelProperty("市级地区id")
    private String cityId;

    @ApiModelProperty("县级地区id")
    private String regionid;

    @ApiModelProperty("详细地址")
    private String address;

    @ApiModelProperty("关联公司信息")
    private List<UserListDetailEnterpriseDTO> linkEnterprises;


}
