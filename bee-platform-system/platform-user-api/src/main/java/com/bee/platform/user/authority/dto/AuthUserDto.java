package com.bee.platform.user.authority.dto;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @Classname AuthUserDto
 * @Description 用户信息
 * @Date 2019/5/27 15:28
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AuthUserDto implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 用户id
     */
    private String userId;
    /**
     * 账号
     */
    private String username;

    /**
     * 用户名
     */
    private String nickname;

    /**
     * 姓名
     */
    private String name;

    /**
     * 用户密码
     */
    private String password;
    /**
     * 用户平台认证token
     */
    private String accessToken;
    /**
     * 用户手机号
     */
    private String phone;
    /**
     * 当前登录的公司id
     */
    private Integer nowCompany;
    /**
     * 当前登录的公司名称
     */
    private String companyName;
    /**
     * 用户头像url
     */
    private String userPhoto;
    /**
     * 职位名称
     */
    private String post;
    /**
     * 邮箱
     */
    private String email;

    /**
     * 县级地区id
     */
    private String regionId;
    /**
     * 子系统token失效时间
     */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Integer expireTime;
    /**
     * QQ账号
     */
    private String qq;
    /**
     * 创建时间
     */
    private Long createTime;
    /**
     * 修改时间
     */
    private Long updateTime;


    public AuthUserDto(AuthPlatformUserInfo user) {
        this.userId = user.getBeesrvId();
        this.username = user.getUsername();
        this.name = user.getName()==null?"":user.getName();
        this.phone = user.getPhone()==null?"":user.getPhone();
        this.userPhoto = user.getHead()==null?"":user.getHead();
        this.email = user.getEmail()==null?"":user.getEmail();
        this.regionId = user.getRegionId()==null?"":user.getRegionId();
        this.qq = user.getQq()==null?"":user.getQq();
        this.post=user.getPost();
        this.nowCompany=user.getOrgId();
        this.companyName=user.getOrg_name();
        this.accessToken=user.getSysToken();
        this.expireTime=user.getExpiresTime();
        this.createTime=user.getCreateTime()==null?System.currentTimeMillis():user.getCreateTime().getTime();
        this.updateTime=user.getUpdateTime()==null?System.currentTimeMillis():user.getUpdateTime().getTime();

    }
}
