package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 炉子档案
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class ErpFurnace extends Model<ErpFurnace> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 所属企业id
     */
    private Integer orgId;
    /**
     * 名称
     */
    private String name;
    /**
     * 状态
     */
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    private Integer deleted;
    /**
     * 操作者id
     */
    private Integer operateId;
    /**
     * 创建者id
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 创建时间
     */
    private Date deletedTime;
    /**
     * 更新时间
     */
    private Date updateTime;




    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
