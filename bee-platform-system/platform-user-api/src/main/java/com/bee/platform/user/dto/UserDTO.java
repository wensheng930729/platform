package com.bee.platform.user.dto;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.UserInfo;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.UUID;

/**
 * Created by CrazyMouse on 2016/11/2.
 * 用户表
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class UserDTO implements Serializable{

    private static final long serialVersionUID = -8321972049304499627L;
    /**
     * 用户id
     */
    private String userId;
    /**
     * 账号
     */
    private String userCode;

    /**
     * 用户名
     */
    private String consumerName;

    /**
     * 姓名
     */
    private String userName;

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
    private String userTel;
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
     * 昵称拼音
     */
    private String nickNamePinyin;

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
    private Long expireTime;
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


    public UserDTO(UserInfo user) {
        this.userId = user.getUuid();
        this.userCode = user.getUsername();
        this.userName = user.getNickname()==null?"":user.getNickname();
        this.userTel = user.getPhone()==null?"":user.getPhone();
        this.userPhoto = user.getHead()==null?"":user.getHead();
        this.email = user.getEmail()==null?"":user.getEmail();
        this.regionId = user.getRegionid()==null?"":user.getRegionid();
        this.qq = user.getQq()==null?"":user.getQq();
        this.post=user.getPost();
        this.nowCompany=user.getOrgId();
        this.companyName=user.getOrg_name();
        this.accessToken=user.getSysToken();
        this.expireTime=user.getExpiresTime();
        this.createTime=user.getCreateAt()==null?System.currentTimeMillis():user.getCreateAt().getTime();
        this.updateTime=user.getUpdateAt()==null?System.currentTimeMillis():user.getUpdateAt().getTime();

    }

    public UserDTO(AuthPlatformUserInfo user) {
        this.userId = UUID.randomUUID().toString();
        this.userCode = user.getUsername();
        this.userName = user.getNickname()==null?"":user.getNickname();
        this.userTel = user.getPhone()==null?"":user.getPhone();
        this.userPhoto = user.getHead()==null?"":user.getHead();
        this.email = user.getEmail()==null?"":user.getEmail();
        this.regionId = user.getRegionId()==null?"":user.getRegionId();
        this.qq = user.getQq()==null?"":user.getQq();
        this.post=user.getPost();
        this.nowCompany=user.getOrgId();
        this.companyName=user.getOrg_name();
        this.accessToken=user.getSysToken();
        this.expireTime=Long.valueOf(user.getExpiresTime());
        this.createTime=user.getCreateTime()==null?System.currentTimeMillis():user.getCreateTime().getTime();
        this.updateTime=user.getUpdateTime()==null?System.currentTimeMillis():user.getUpdateTime().getTime();

    }
}
