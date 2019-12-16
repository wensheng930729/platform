package com.bee.platform.datadriver.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 产品档案
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@Accessors(chain = true)
@NoArgsConstructor
public class ErpProduct extends Model<ErpProduct> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 创建人企业id
     */
    private Integer enterpriseId;
    /**
     * 产品编码
     */
    private String code;
    /**
     * 产品logo
     */
    private String logo;
    /**
     * 产品名称
     */
    private String name;
    /**
     * 产品检测属性
     */
    private String checkItems;
    /**
     * 单位
     */
    private String unit;
    /**
     * 产品类别
     */
    private Integer category;
    /**
     * 启用批次，1-启用，0-不启用
     */
    private Integer enableBatch;
    /**
     * 状态:1-启用,0-禁用
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
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 删除时间
     */
    private Date deletedTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
