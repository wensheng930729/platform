package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 仓库档案
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@Accessors(chain = true)
public class ErpRepository extends Model<ErpRepository> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 名称
     */
    private String name;
    /**
     * 状态
     */
    private Integer status;
    /**
     * 所属企业id
     */
    private Integer orgId;
    
    /**
     * 删除状态
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
