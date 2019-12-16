package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

/**
 * @Classname EditAuthPlatformUserRQ
 * @Description 修改用户信息
 * @Date 2019/5/24 14:07
 * @Author xin.huang
 */
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("修改用户信息")
public class EditAuthPlatformUserRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("姓名")
    @NotEmpty(message = "姓名不能为空")
    @Length(max = 20,message = "姓名20个字符")
    private String name;

    @ApiModelProperty("用户名")
    private String nickname;

    @ApiModelProperty("地区")
    private String regionId;

    @ApiModelProperty("详细地址")
    private String address;

    @ApiModelProperty("联系电话")
    private String fixtel;

    @ApiModelProperty("头像地址")
    private String head;

    @ApiModelProperty("邮箱")
    private  String email;
}
