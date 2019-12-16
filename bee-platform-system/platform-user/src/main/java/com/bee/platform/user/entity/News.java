package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @notes  新闻
 * @Author junyang.li
 * @Date 10:48 2019/3/22
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("news")
public class News extends Model<News> {

    private static final long serialVersionUID = 1L;
    /**
     * 资讯id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     *  资讯标题
     */
    private String title;

    /**
     * 资讯图片(无用)
     */
    private String image;
    /**
     * 资讯来源
     */
    private String newsSource;
    /**
     * 资讯创建日期
     */
    private Date createAt;
    /**
     * 资讯修改日期
     */
    private Date updateAt;
    /**
     * 资讯发布者id
     */
    private Integer userId;
    /**
     * 资讯点击量
     */
    private Integer hits;
    /**
     * 资讯类型
     */
    private Integer type;
    /**
     * 是否软删除，0-未删除，1-已删除
     */
    private Integer state;
    /**
     * 资讯内容
     */
    private String content;

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        }
        if (that == null) {
            return false;
        }
        if (getClass() != that.getClass()) {
            return false;
        }
        News other = (News) that;
        return (this.getId() == null ? other.getId() == null : this.getId().equals(other.getId()))
            && (this.getTitle() == null ? other.getTitle() == null : this.getTitle().equals(other.getTitle()))
            && (this.getImage() == null ? other.getImage() == null : this.getImage().equals(other.getImage()))
            && (this.getNewsSource() == null ? other.getNewsSource() == null : this.getNewsSource().equals(other.getNewsSource()))
            && (this.getCreateAt() == null ? other.getCreateAt() == null : this.getCreateAt().equals(other.getCreateAt()))
            && (this.getUpdateAt() == null ? other.getUpdateAt() == null : this.getUpdateAt().equals(other.getUpdateAt()))
            && (this.getUserId() == null ? other.getUserId() == null : this.getUserId().equals(other.getUserId()))
            && (this.getHits() == null ? other.getHits() == null : this.getHits().equals(other.getHits()))
            && (this.getType() == null ? other.getType() == null : this.getType().equals(other.getType()))
            && (this.getState() == null ? other.getState() == null : this.getState().equals(other.getState()))
            && (this.getContent() == null ? other.getContent() == null : this.getContent().equals(other.getContent()));
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((getId() == null) ? 0 : getId().hashCode());
        result = prime * result + ((getTitle() == null) ? 0 : getTitle().hashCode());
        result = prime * result + ((getImage() == null) ? 0 : getImage().hashCode());
        result = prime * result + ((getNewsSource() == null) ? 0 : getNewsSource().hashCode());
        result = prime * result + ((getCreateAt() == null) ? 0 : getCreateAt().hashCode());
        result = prime * result + ((getUpdateAt() == null) ? 0 : getUpdateAt().hashCode());
        result = prime * result + ((getUserId() == null) ? 0 : getUserId().hashCode());
        result = prime * result + ((getHits() == null) ? 0 : getHits().hashCode());
        result = prime * result + ((getType() == null) ? 0 : getType().hashCode());
        result = prime * result + ((getState() == null) ? 0 : getState().hashCode());
        result = prime * result + ((getContent() == null) ? 0 : getContent().hashCode());
        return result;
    }

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}