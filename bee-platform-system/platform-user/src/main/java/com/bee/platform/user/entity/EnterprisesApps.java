package com.bee.platform.user.entity;

import java.io.Serializable;
import java.util.Date;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @notes 企业可访问app列表
 * @Author junyang.li
 * @Date 11:00 2019/3/22
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("enterprises_apps")
public class EnterprisesApps extends Model<EnterprisesApps> {

    private static final long serialVersionUID = 1L;
    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 公司主键
     */
    private Integer orgId;
    /**
     * 公司名称
     */
    private String orgName;
    /**
     * 产品主键
     */
    private Integer appId;
    /**
     * 产品角色id
     */
    private Integer appRolesId;
    /**
     * 审核状态(0: 未通过，1: 已通过, 2: 审核中)
     */
    private Integer aduitState;
    /**
     * 拒绝原因
     */
    private String rejectReason;
    private String url;
    /**
     * 数据状态，0-无效，1-有效
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 创建人
     */
    private String creator;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 修改人
     */
    private String modifier;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
